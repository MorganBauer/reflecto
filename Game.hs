{-
    SwitchGame.hs
    The main file for the switch game.
    Contains the main function and idle callback code.
    
    © 2011 Stephen Cave
-}

module Main where

import Graphics.UI.GLUT hiding (initState)

import GameState
import Display
import Keyboard
import GameLogic


main :: IO ()
main = do
    (progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, RGBMode]
    initialWindowSize $= Size 800 600
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
    let getKey k = get . k . keyboard
    w  <- getKey wKey gstate
    s  <- getKey sKey gstate
    a  <- getKey aKey gstate
    d  <- getKey dKey gstate
    l  <- getKey left gstate
    l' <- getKey left' gstate
    r  <- getKey right gstate
    r' <- getKey right' gstate
    let fx = if (a == Down) && (d == Up) then (-) else
             if (a == Up) && (d == Down) then (+) else const
    let fy = if (s == Down) && (w == Up) then (-) else
             if (s == Up) && (w == Down) then (+) else const
    (xPos . player) gstate $~ (\x -> fx x 3)
    (yPos . player) gstate $~ (\y -> fy y 3)
    let rotate = if (l == Down) && (l' == Up) && (r == Up) then clockwise else
                 if (l == Up) && (r == Down) && (r' == Up) then cclockwise else id
    (orientation . player) gstate $~ rotate
    (left' . keyboard) gstate $= l
    (right' . keyboard) gstate $= r
    postRedisplay Nothing
    addTimerCallback stepTime $ timer gstate
