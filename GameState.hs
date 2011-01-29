{-
    GameState.hs
    Defines the state structures and initialization code
    for the game.

    © 2011 Stephen Cave
-}

module GameState where

import Control.Monad
import Graphics.UI.GLUT
import Data.IORef

pixelsPerSquare :: (Num a) => a
pixelsPerSquare = 50

class Movable a where
    getCoord :: a -> IO (GLint, GLint)
    setCoord :: a -> (GLint, GLint) -> IO ()
    getLocation :: a -> IO (GLfloat, GLfloat)
    facing :: a -> IORef Orientation

instance Movable PlayerState where
    getCoord player = do
        x <- liftM toGrid $ (get . xPos) player
        y <- liftM toGrid $ (get . yPos) player
        return (x,y)
      where 
        toGrid z = floor (z/pixelsPerSquare)

    setCoord player (x,y) = do
        --note: +0.5 will center the player on that grid tile.
        xPos player $= pixelsPerSquare * ( fromIntegral x + 0.5 )
        yPos player $= pixelsPerSquare * ( fromIntegral y + 0.5 )

    getLocation player = do
        x <- (get . xPos) player
        y <- (get . yPos) player
        return (x,y)

    facing = orientation


--GameState: the collection of all states of the game
--      including keyboard, player, and world.
data GameState = GameState { player :: PlayerState
                           , keyboard :: KeyboardState
                           }

--PlayerState: includes information like position, orientation, etc.
data PlayerState = PlayerState {xPos, yPos :: IORef GLfloat, orientation :: IORef Orientation}


--KeyboardState: indicates which keys are pressed and which are not.
--      True -> Pressed
data KeyboardState = KeyboardState {wKey, sKey, aKey, dKey, left, left', right, right' :: IORef KeyState}

--an object's orientation
data Orientation = North
                 | Northwest
                 | West
                 | Southwest
                 | South
                 | Southeast
                 | East
                 | Northeast
             deriving Eq

--creates a new GameState structure with predetermined initial values
initState :: IO GameState
initState = do
    w     <- newIORef Up
    s     <- newIORef Up
    a     <- newIORef Up
    d     <- newIORef Up
    lft   <- newIORef Up
    lft'  <- newIORef Up
    rght  <- newIORef Up
    rght' <- newIORef Up
    x     <- newIORef 200
    y     <- newIORef 200
    or    <- newIORef North
    let ks = KeyboardState 
            { wKey   = w
            , sKey   = s
            , aKey   = a
            , dKey   = d
            , left   = lft
            , left'  = lft'
            , right  = rght
            , right' = rght'
            }
        ps = PlayerState
            { xPos = x
            , yPos = y
            , orientation = or
            }
    return $ GameState
            { player = ps
            , keyboard = ks
            }

