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
        b1 = Block
            { xPos = 325
            , yPos = 425
            , orientation = North
            , reflected = False
            }
        r1 = Roller
            { xPos = 325
            , yPos = 125
            , orientation = North
            , reflected = False
            , moving = Nothing
            }
        b2 = Block
            { xPos = 525
            , yPos = 125
            , orientation = North
            , reflected = False
            }
        b3 = Block
            { xPos = 575
            , yPos = 575
            , orientation = North
            , reflected = False
            }
        w1 = Wall
            { xPos = 25
            , yPos = 25
            }
        w2 = Wall
            { xPos = 75
            , yPos = 25
            }
        w3 = Wall
            { xPos = 575
            , yPos = 475
            }
        p1 = Pit
            { xPos = 125
            , yPos = 425
            }
        p2 = Pit
            { xPos = 175
            , yPos = 425
            }
    ks <- newIORef k
    ps <- newIORef p
    os <- newIORef [b1,r1,b2,b3,w1,w2,w3,p1,p2]
    return $ GameState
            { player = ps
            , objects = os
            , keyboard = ks
            }

