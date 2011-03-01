{-
    SwitchGame.hs
    The main file for the switch game.
    Contains the main function and idle callback code.
    
    © 2011 Stephen Cave
-}

module Main where

import Graphics.UI.GLUT hiding (initState,position)
import Control.Monad (liftM)
import Data.Maybe (isJust,fromJust,isNothing)

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

updatePlayer :: Keyboard -> [GObject] -> GObject -> GObject
updatePlayer k os p = p{ xPos = x
                       , yPos = y
                       , sightLength = getSL
                       , target = t
                       , orientation = o
                       }
    where
        x' = if and [aKey k == Down, dKey k == Up] then (xPos p) - vel else
             if and [aKey k == Up, dKey k == Down] then (xPos p) + vel else xPos p
        y' = if and [sKey k == Down, wKey k == Up] then (yPos p) - vel else
             if and [sKey k == Up, wKey k == Down] then (yPos p) + vel else yPos p
        x = if or [x' > mapWidth,  x' < 0, intersection (x',yPos p) os] then xPos p else x'
        y = if or [y' > mapHeight, y' < 0, intersection (xPos p,y') os] then yPos p else y'
        t = findTarget os (viewCheckList p{xPos=x,yPos=y,orientation=o})
        getSL = case t of
            Just ob -> edgeShape ob (x,y) o
            Nothing -> 1000
        o = if and [qKey k == Down, qKey' k == Up, eKey k == Up] then cclockwise (orientation p) else
            if and [qKey k == Up, eKey k == Down, eKey' k == Up] then clockwise (orientation p) else orientation p

reflecto :: GameState -> IO ()
reflecto gstate = do
    k <- (get . keyboard) gstate
    p <- (get . player) gstate
    os <- (get . blocks) gstate
    if not $ and [space k == Down, space' k == Up, isJust $ target p, movep $ fromJust $ target p] then return ()
      else do
        let t = (fromJust . target) p --note: fromJust is safe because of isJust check above.
            (px,py) = position t
            oo = orientation t
            po = orientation p
            (ox,oy) = (xPos p, yPos p)
            obj = move t (ox,oy) (reorient oo (orientation p))
            os' = obj{reflected=not(reflected obj)} : filter (/= t) os
        player gstate $= p{xPos=px,yPos=py,orientation=clockwise4 po,reflected=not $ reflected p}
        blocks gstate $= os'
    where
        reorient oo po = if po `elem` [North,South] then clockwise4 oo else
                         if po `elem` [Northeast,Southwest] then clockwise2 oo else
                         if po `elem` [East,West] then oo else cclockwise2 oo
