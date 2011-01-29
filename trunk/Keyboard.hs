{-
    SwitchKeyboard.hs
    The keyboard callback

    © 2011 Stephen Cave
-}

module Keyboard where

import Graphics.UI.GLUT
import System.Exit

import GameState

keyboardMouse :: KeyboardState -> KeyboardMouseCallback
keyboardMouse kstate (Char c) pos _ _ = case c of
    'w'   -> update wKey
    'a'   -> update aKey
    's'   -> update sKey
    'd'   -> update dKey
    'q'   -> update left
    'e'   -> update right
    '\27' -> exitWith ExitSuccess
    _     -> return ()
    where update key = do
            key kstate $= pos
            return ()
          {-update' k1 k2 = do
            kst <- (get . k1) kstate
            print kst
            print pos
            k2 kstate $= kst
            k1 kstate $= pos
            return ()-}
{-
keyboardMouse kstate (SpecialKey k) pos _ _ = case k of
    KeyLeft  -> update left
    keyRight -> update right
    _        -> return ()
    where update key = do
            key kstate $= pos
            return ()
            -}
keyboardMouse _ _ _ _ _ = return ()
