{-
    GameState.hs
    Defines the state structures and initialization code
    for the game.

    © 2011 Stephen Cave
-}

module GameState where

import Graphics.UI.GLUT
import Data.IORef
import GameLogic

--GameState: the collection of all states of the game
--      including keyboard, player, and world.
data GameState = GameState { player :: IORef GObject
                           , objects :: IORef [GObject]
                           , keyboard :: IORef Keyboard
                           }


--Keyboard: indicates which keys are pressed and which are not.
data Keyboard = Keyboard { wKey
                         , sKey
                         , aKey
                         , dKey
                         , qKey
                         , qKey'
                         , eKey
                         , eKey'
                         , space
                         , space' :: KeyState
                         }

--creates a new GameState structure with predetermined initial values
initState :: IO GameState
initState = do
    let k = Keyboard 
            { wKey   = Up
            , sKey   = Up
            , aKey   = Up
            , dKey   = Up
            , qKey   = Up
            , qKey'  = Up
            , eKey   = Up
            , eKey'  = Up
            , space  = Up
            , space' = Up
            }
        p = Player
            { xPos = 200
            , yPos = 200
            , velocity = Nothing
            , sightLength = 0
            , target = Nothing
            , orientation = North
            , reflected = False
            }
    objs <- readLevel "level1"
    ks <- newIORef k
    ps <- newIORef p
    os <- newIORef (map reposition objs)
    return $ GameState
            { player = ps
            , objects = os
            , keyboard = ks
            }


readLevel :: String -> IO ([GObject])
readLevel file = do
    str <- readFile file    
    return $ map read $ lines str
