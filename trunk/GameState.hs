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

pixelsPerSquare :: (Num a) => a
pixelsPerSquare = 50

--GameState: the collection of all states of the game
--      including keyboard, player, and world.
data GameState = GameState { player :: IORef MObject
                           , blocks :: IORef [MObject]
                           , keyboard :: IORef Keyboard
                           }

--MObject: Mobile Objects. Type includes information like position, orientation, etc.
data MObject = Player { xPos :: GLdouble 
                      , yPos :: GLdouble 
                      , orientation :: Orientation 
                      , reflected :: Bool 
                      } |
               Block { xPos :: GLdouble 
                     , yPos :: GLdouble 
                     , orientation :: Orientation 
                     , reflected :: Bool 
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
            , orientation = North
            , reflected = False
            }
        b = Block
            { xPos = 400+pixelsPerSquare/2
            , yPos = 400+pixelsPerSquare/2
            , orientation = North
            , reflected = False
            }
    ks <- newIORef k
    ps <- newIORef p
    bs <- newIORef [b]
    return $ GameState
            { player = ps
            , blocks = bs
            , keyboard = ks
            }

