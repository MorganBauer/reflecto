{-
    GameState.hs
    Defines the state structures and initialization code
    for the game.

    © 2011 Stephen Cave
-}

module GameState where

import Graphics.UI.GLUT
import System.Directory
import Data.IORef

import GameLogic

--GameState: the collection of all states of the game
--      including keyboard, player, and world.
data GameState = GameState { player :: IORef GObject
                           , objects :: IORef [GObject]
                           , keyboard :: IORef Keyboard
                           , level :: IORef Integer
                           , levelh :: IORef [(Vertex2 GLint,String)]
                           , phase :: IORef GamePhase
                           , cursorPos :: IORef Integer
                           }


--Keyboard: indicates which keys are pressed and which are not.
data Keyboard = Keyboard { wKey
                         , wKey'
                         , sKey
                         , sKey'
                         , aKey
                         , dKey
                         , qKey
                         , qKey'
                         , eKey
                         , eKey'
                         , rKey
                         , space
                         , space'
                         , esc :: KeyState
                         }

--creates a new GameState structure with predetermined initial values
initState :: IO GameState
initState = do
    let k = Keyboard 
            { wKey   = Up
            , wKey'  = Up
            , sKey   = Up
            , sKey'  = Up
            , aKey   = Up
            , dKey   = Up
            , qKey   = Up
            , qKey'  = Up
            , eKey   = Up
            , eKey'  = Up
            , rKey   = Up
            , space  = Up
            , space' = Up
            , esc    = Up
            }
        p = Player
            { xPos = 0
            , yPos = 0
            , velocity = Nothing
            , sightLength = 0
            , target = []
            , orientation = North
            , reflected = False
            }
        l = 1
    rawObjs <- readLevel "level1"
    h <- readHelp "level1h"
    let objs = map reposition rawObjs
        start = head $ filter isStart objs
    ks <- newIORef k
    ps <- newIORef p{ xPos = xPos start
                    , yPos = yPos start
                    , orientation = orientation start
                    }
    os <- newIORef objs
    lvl <- newIORef l
    helps <- newIORef h
    phz <- newIORef Title
    crs <- newIORef 1
    return $ GameState
            { player = ps
            , objects = os
            , keyboard = ks
            , level = lvl
            , levelh = helps
            , phase = phz
            , cursorPos = crs
            }

readHelp :: String -> IO ([(Vertex2 GLint,String)])
readHelp file = do
    b <- doesFileExist file
    if b then do
        str <- readFile file
        return (read str)
      else return []

readLevel :: String -> IO ([GObject])
readLevel file = do
    str <- readFile file    
    objs <- return $ map read $ lines str
    if (length . filter isStart) objs /= 1 then error "Multiple start locations unsupported"
        else return objs
 
isStart o = case o of
    Start{} -> True
    otherwise -> False

isEnd o = case o of
    End{} -> True
    otherwise -> False
