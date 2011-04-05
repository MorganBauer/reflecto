{-
    SwitchKeyboard.hs
    The keyboard callback

    © 2011 Stephen Cave
-}

module Keyboard where

import Data.IORef
import Graphics.UI.GLUT

import GameState

keyboardMouse :: IORef Keyboard -> KeyboardMouseCallback
keyboardMouse kstate (Char c) pos _ _ = case c of
    'w'   -> kstate $~ (\k -> k{wKey=pos})
    'a'   -> kstate $~ (\k -> k{aKey=pos})
    's'   -> kstate $~ (\k -> k{sKey=pos})
    'd'   -> kstate $~ (\k -> k{dKey=pos})
    'q'   -> kstate $~ (\k -> k{qKey=pos})
    'e'   -> kstate $~ (\k -> k{eKey=pos})
    'r'   -> kstate $~ (\k -> k{rKey=pos})
    ' '   -> kstate $~ (\k -> k{space=pos})
    '\27' -> kstate $~ (\k -> k{esc=pos})
    _     -> return ()

keyboardMouse _ _ _ _ _ = return ()
