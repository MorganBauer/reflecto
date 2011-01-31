{-
    SwitchGame.hs
    The main file for the switch game.
    Contains the main function and idle callback code.
    
    © 2011 Stephen Cave
-}

module Main where

import Graphics.UI.GLUT hiding (initState)
import Control.Monad (liftM)

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
    let getKey k = liftM k . get . keyboard
    w   <- getKey wKey   gstate
    s   <- getKey sKey   gstate
    a   <- getKey aKey   gstate
    d   <- getKey dKey   gstate
    q   <- getKey qKey   gstate
    q'  <- getKey qKey'  gstate
    e   <- getKey eKey   gstate
    e'  <- getKey eKey'  gstate
    sp  <- getKey space  gstate
    sp' <- getKey space' gstate
    let fx = if (a == Down) && (d == Up) then (-) else
             if (a == Up) && (d == Down) then (+) else const
    let fy = if (s == Down) && (w == Up) then (-) else
             if (s == Up) && (w == Down) then (+) else const
    player gstate $~ (\p@(Player{xPos=x}) -> p{xPos=fx x 3})
    player gstate $~ (\p@(Player{yPos=y}) -> p{yPos=fy y 3})
    let rotate = if (q == Down) && (q' == Up) && (e == Up) then clockwise else
                 if (q == Up) && (e == Down) && (e' == Up) then cclockwise else id
    player gstate $~ (\p@(Player{orientation=o}) -> p{orientation=rotate o})
    keyboard gstate $~ (\k@(Keyboard{qKey=q}) -> k{qKey'=q})
    keyboard gstate $~ (\k@(Keyboard{eKey=e}) -> k{eKey'=e})
    let ref = if (sp == Down) && (sp' == Up) then not else id
    player gstate $~ (\p@(Player{reflected=r}) -> p{reflected=ref r})
    keyboard gstate $~ (\k@(Keyboard{space=s}) -> k{space'=s})
    postRedisplay Nothing
    addTimerCallback stepTime $ timer gstate
