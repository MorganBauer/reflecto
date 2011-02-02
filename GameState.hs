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
data GameState = GameState { player :: IORef MObject
                           , blocks :: IORef [MObject]
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
            , sightLength = 400
            , target = Nothing
            , orientation = North
            , reflected = False
            }
        b1 = Block
            { xCoord = 6
            , yCoord = 8
            , orientation = North
            , reflected = False
            }
        b2 = Block
            { xCoord = 10
            , yCoord = 2
            , orientation = North
            , reflected = False
            }
        b3 = Block
            { xCoord = 11
            , yCoord = 11
            , orientation = North
            , reflected = False
            }
    ks <- newIORef k
    ps <- newIORef p
    bs <- newIORef [b1,b2,b3]
    return $ GameState
            { player = ps
            , blocks = bs
            , keyboard = ks
            }

