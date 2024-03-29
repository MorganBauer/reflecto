{-
    SwitchGame.hs
    The main file for the switch game.
    Contains the main function and idle callback code.
    
    © 2011 Stephen Cave
-}

module Main where

import Graphics.UI.GLUT hiding (initState,position,Menu)
import Control.Monad 
import System.Directory
import Data.Maybe (isJust,fromJust,isNothing)
import System.Exit

import GameState
import Display
import Keyboard
import GameLogic


main :: IO ()
main = do
    (progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, RGBMode]
    initialWindowSize $= Size mapWidth mapHeight
    createWindow progName
    clearColor $= Color4 0.1 0.3 0.1 0
    gstate <- initState
    displayCallback $= display gstate
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just (keyboardMouse $ keyboard gstate)
    addTimerCallback stepTime $ timer gstate
    mainLoop

--time between updates, in ms
-- 16 ms ~ 1/60 s
stepTime :: Timeout
stepTime = 16

timer :: GameState -> TimerCallback
timer gstate = do
    phz <- (get . phase) gstate
    case phz of
        Play -> playMode gstate
        Title -> titleMode gstate
        Menu -> menuMode gstate
        Win -> winMode gstate

playMode gstate = do
    k <- (get . keyboard) gstate
    p <- (get . player) gstate
    os <- (get . objects) gstate
    player gstate $~ (updatePlayer k os)
    reflecto gstate
    pushing gstate
    objects gstate $~ map moveUpdate
    objects gstate $~ fillPits
    objects gstate $~ filter (not . isBeam)
    objects gstate $~ makeBeams
    objects gstate $~ beamExtend
    objects gstate $~ activeUpdate p
    objects gstate $~ doorUpdate
    keyboard gstate $~ (\k@(Keyboard{space=s}) -> k{space'=s})
    keyboard gstate $~ (\k@(Keyboard{qKey=q}) -> k{qKey'=q})
    keyboard gstate $~ (\k@(Keyboard{eKey=e}) -> k{eKey'=e})
    keyboard gstate $~ (\k@(Keyboard{wKey=w}) -> k{wKey'=w})
    keyboard gstate $~ (\k@(Keyboard{sKey=s}) -> k{sKey'=s})
    keyboard gstate $~ (\k@(Keyboard{aKey=a}) -> k{aKey'=a})
    keyboard gstate $~ (\k@(Keyboard{dKey=d}) -> k{dKey'=d})
    updateLevel gstate
    postRedisplay Nothing
    if esc k == Down then phase gstate $= Menu else return ()
    addTimerCallback stepTime $ timer gstate

titleMode gstate = do
    k <- (get . keyboard) gstate
    if space k == Down && space' k == Up then phase gstate $= Play else return ()
    keyboard gstate $~ (\k@(Keyboard{space=s}) -> k{space'=s})
    postRedisplay Nothing
    addTimerCallback stepTime $ timer gstate

menuMode gstate = do
    k <- (get . keyboard) gstate
    cursor <- (get . cursorPos) gstate
    clvl <- (get . cursorLevel) gstate
    low <- doesFileExist $ "level" ++ show (clvl-1)
    high <- doesFileExist $ "level" ++ show (clvl+1)
    if wKey k == Down && wKey' k == Up then cursorPos gstate $~ (\x -> if x < 2 then 1 else x-1) else return () 
    if sKey k == Down && sKey' k == Up then cursorPos gstate $~ (\x -> if x > 3 then 4 else x+1) else return () 
    if aKey k == Down && aKey' k == Up && low && cursor == 3 then cursorLevel gstate $~ pred else return ()
    if dKey k == Down && dKey' k == Up && high && cursor == 3 then cursorLevel gstate $~ succ else return ()
    if space k == Down then do
        cpos <- get $ cursorPos gstate
        case cpos of
            1 -> do 
                phase gstate $= Play
            2 -> do
                phase gstate $= Play
                level gstate $= 1
                cursorPos gstate $= 1
                rawObjs <- readLevel "level1"
                h <- readHelp "level1h"
                let objs = map reposition rawObjs
                    start = head $ filter isStart objs
                objects gstate $= objs
                levelh gstate $= h
                player gstate $~ (\p -> p{xPos = xPos start ,yPos = yPos start ,orientation = orientation start})
            3 -> do
                phase gstate $= Play
                level gstate $= clvl
                cursorPos gstate $= 1
                rawObjs <- readLevel $ "level" ++ show clvl
                h <- readHelp $ "level" ++ show clvl ++ "h"
                let objs = map reposition rawObjs
                    start = head $ filter isStart objs
                objects gstate $= objs
                levelh gstate $= h
                player gstate $~ (\p -> p{xPos = xPos start ,yPos = yPos start ,orientation = orientation start})
            4 -> exitWith ExitSuccess
            _ -> return ()
      else return ()
    keyboard gstate $~ (\k -> k{space=Up,space'=Up})
    keyboard gstate $~ (\k@(Keyboard{wKey=w}) -> k{wKey'=w})
    keyboard gstate $~ (\k@(Keyboard{sKey=s}) -> k{sKey'=s})
    keyboard gstate $~ (\k@(Keyboard{aKey=a}) -> k{aKey'=a})
    keyboard gstate $~ (\k@(Keyboard{dKey=d}) -> k{dKey'=d})
    postRedisplay Nothing
    addTimerCallback stepTime $ timer gstate

winMode gstate = do
    k <- (get . keyboard) gstate
    if space k == Down then phase gstate $= Title else return ()
    keyboard gstate $~ (\k@(Keyboard{space=s}) -> k{space'=s})
    postRedisplay Nothing
    addTimerCallback stepTime $ timer gstate

updatePlayer :: Keyboard -> [GObject] -> GObject -> GObject
updatePlayer k os p = p{ xPos = x
                       , yPos = y
                       , velocity = getVel
                       , sightLength = getSL
                       , target = t
                       , orientation = o
                       }
    where
        x' = if and [aKey k == Down, dKey k == Up] then (xPos p) - vel else
             if and [aKey k == Up, dKey k == Down] then (xPos p) + vel else xPos p
        y' = if and [sKey k == Down, wKey k == Up] then (yPos p) - vel else
             if and [sKey k == Up, wKey k == Down] then (yPos p) + vel else yPos p
        obstaclex = intersection (freeToGrid (x', yPos p)) os
        obstaclex' = intersection (freeToGrid (x'+pixelsPerSquare * signum (x'-xPos p), yPos p)) os
        obstacley = intersection (freeToGrid (xPos p, y')) os
        obstacley' = intersection (freeToGrid (xPos p, y'+pixelsPerSquare * signum (y'-yPos p))) os
        x = if or [x' > mapWidth,  x' < 0, obstructs obstaclex obstaclex' 'x'] then xPos p else x'
        y = if or [y' > mapHeight, y' < 0, obstructs obstacley obstacley' 'y'] then yPos p else y'
        t = findTarget os (f p{xPos=x,yPos=y,orientation=o})
        f = if (x,y) == gridToFree (freeToGrid (x,y)) then viewCheckList' else viewCheckList
        getSL = case t of
            --Declared fixed. All contours follow grid.
            (ob:obs) -> edgeShape ob (x,y) o
            [] -> 1000
        o = if and [qKey k == Down, qKey' k == Up, eKey k == Up] then cclockwise (orientation p) else
            if and [qKey k == Up, eKey k == Down, eKey' k == Up] then clockwise (orientation p) else orientation p
        getVel = case (signum (x-xPos p), signum (y-yPos p)) of
            ( 0, 0) -> Nothing
            ( 0, 1) -> Just North
            ( 1, 0) -> Just East
            ( 0,-1) -> Just South
            (-1, 0) -> Just West
            ( 1, 1) -> Just Northeast
            ( 1,-1) -> Just Southeast
            (-1,-1) -> Just Southwest
            (-1, 1) -> Just Northwest

reflecto :: GameState -> IO ()
reflecto gstate = do
    k <- (get . keyboard) gstate
    p <- (get . player) gstate
    os <- (get . objects) gstate
    if not $ and [space k == Down, space' k == Up, (not . null) $ target p, all movep $ target p] then return ()
      else do
        let t = (head . target) p --note: head is safe because of null check above.
            (px,py) = position t
            oo = orientation t
            r = reflected t
            po = orientation p
            (ox,oy) = (xPos p, yPos p)
            obj = move t (ox,oy) (reorient oo (orientation p)) (not r)
            os' = obj : filter (/= t) os
        player gstate $= p{xPos=px,yPos=py,orientation=clockwise4 po,reflected=not $ reflected p}
        objects gstate $= os'
    where
        reorient oo po = if po == oo || po == clockwise4 oo then clockwise4 oo else
                         if cclockwise po == oo || cclockwise po == clockwise4 oo then cclockwise2 oo else
                         if clockwise po == oo || clockwise po == clockwise4 oo then clockwise2 oo else oo

pushing :: GameState -> IO ()
pushing gstate = do
    p <- (get . player) gstate
    os <- (get . objects) gstate
    let mob = filter pushp $ intersection (coords p) os
    if not $ and [not $ null mob, isJust (velocity p)] then return ()
      else do
        let os' = filter (not . (`elem` mob)) os
            moveDir o = limitedVel o $ velocity p
            x' o = xPos o + case moveDir o of
                Just East -> pixelsPerSquare
                Just West -> -pixelsPerSquare
                _ -> 0
            y' o = yPos o + case moveDir o of
                Just North -> pixelsPerSquare
                Just South -> -pixelsPerSquare
                _ -> 0
            setMoving o = o{xPos = x' o, yPos = y' o, moving = moveDir o}
            ob' = map setMoving mob
        objects gstate $= ob' ++ os'

updateLevel :: GameState -> IO ()
updateLevel gstate = do
    k <- (get . keyboard) gstate
    p <- (get . player) gstate
    o <- (get . objects) gstate
    let os = intersection (coords p) o
    if any isEnd os 
        then do level gstate $~ succ
                l <- (get . level) gstate
                let filename = "level" ++ show l
                b <- doesFileExist filename
                if b then do
                    rawObjs <- readLevel filename
                    let objs = map reposition rawObjs
                        start = head $ filter isStart objs
                    objects gstate $= objs
                    player gstate $~ (\p -> p{xPos = xPos start ,yPos = yPos start ,orientation = orientation start})
                    let helpname = "level" ++ show l ++ "h"
                    help <- readHelp helpname
                    levelh gstate $= help
                  else do
                    phase gstate $= Win
                    level gstate $= 1
                    rawObjs <- readLevel "level1"
                    let objs = map reposition rawObjs
                        start = head $ filter isStart objs
                    objects gstate $= objs
                    player gstate $~ (\p -> p{xPos = xPos start ,yPos = yPos start ,orientation = orientation start})
                    help <- readHelp "level1h"
                    levelh gstate $= help
        else return ()
    if rKey k == Down
        then do l <- (get . level) gstate
                let filename = "level" ++ show l
                b <- doesFileExist filename
                if b then do
                    rawObjs <- readLevel filename
                    let objs = map reposition rawObjs
                        start = head $ filter isStart objs
                    objects gstate $= objs
                    player gstate $~ (\p -> p{xPos = xPos start ,yPos = yPos start ,orientation = orientation start})
                  else do
                      phase gstate $= Win
                      level gstate $= 1
                let helpname = "level" ++ show l ++ "h"
                help <- readHelp helpname
                levelh gstate $= help
        else return ()
