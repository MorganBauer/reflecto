{-
    SwitchGame.hs
    The main file for the switch game.
    Contains the main function and idle callback code.
    
    © 2011 Stephen Cave
-}

module Main where

import Graphics.UI.GLUT hiding (initState)
import Control.Monad (liftM)
import Data.Maybe (isJust,fromJust)

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
    clearColor $= Color4 0 0 0 0
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
    k <- (get . keyboard) gstate
    p <- (get . player) gstate
    os <- (get . blocks) gstate
    player gstate $~ (updatePlayer k os)
    reflecto gstate
    keyboard gstate $~ (\k@(Keyboard{space=s}) -> k{space'=s})
    keyboard gstate $~ (\k@(Keyboard{qKey=q}) -> k{qKey'=q})
    keyboard gstate $~ (\k@(Keyboard{eKey=e}) -> k{eKey'=e})
    postRedisplay Nothing
    addTimerCallback stepTime $ timer gstate

updatePlayer :: Keyboard -> [MObject] -> MObject -> MObject
updatePlayer k os p = Player { xPos = x
                             , yPos = y
                             , sightLength = getSL
                             , target = t
                             , orientation = rot o
                             , reflected = ref (reflected p)
                             }
    where
        x = if aKey k == Down && dKey k == Up then (xPos p) - vel else
            if aKey k == Up && dKey k == Down then (xPos p) + vel else xPos p
        y = if sKey k == Down && wKey k == Up then (yPos p) - vel else
            if sKey k == Up && wKey k == Down then (yPos p) + vel else yPos p
        t = findTarget os (viewCheckList p{xPos=x,yPos=y,orientation=o})
        getSL = case t of
            Just Block{xCoord=xc,yCoord=yc} -> let (xc', yc') = gridToFree (xc,yc) 
                        in (if o `elem` [North,South,East,West] then 1 
                            else (sqrt 2)) * (max (abs $ x - xc') (abs $ y - yc') - pixelsPerSquare/2)
            Nothing -> 1000
        o = if and [qKey k == Down, qKey' k == Up, eKey k == Up] then cclockwise (orientation p) else
            if and [qKey k == Up, eKey k == Down, eKey' k == Up] then clockwise (orientation p) else orientation p
        (ref,rot) = if and [space k == Down, space' k == Up, isJust t] then (not,clockwise4) else (id,id)

reflecto :: GameState -> IO ()
reflecto gstate = do
    k <- (get . keyboard) gstate
    p <- (get . player) gstate
    os <- (get . blocks) gstate
    if not $ and [space k == Down, space' k == Up, isJust (target p)] then return ()
     else do
        let t = (fromJust . target) p --note: fromJust is safe because of isJust check above.
            (px,py) = getFree t
            (ox,oy) = (xPos p, yPos p)
            os' = move t ox oy : filter (/= t) os
        player gstate $= p{xPos=px,yPos=py}
        blocks gstate $= os'
    where
        getFree o = case o of
            Block{xCoord=x, yCoord=y} -> gridToFree (x,y)
            otherwise -> error $ "Unsupported MObject constructor: " ++ show o
        move o x y = let (x',y') = freeToGrid (x,y) 
            in case o of
                b@Block{} -> b{xCoord=x',yCoord=y'}
                otherwise -> error $ "Unsupported MObject constructor: " ++ show o
