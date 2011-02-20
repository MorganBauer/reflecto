{-
    GameLogic.hs
    The game logic for the switching game

    © 2011 Stephen Cave
-}

module GameLogic where

import Graphics.UI.GLUT
import Data.List (find)
import Data.Maybe (isJust)

pixelsPerSquare :: (Num a) => a
pixelsPerSquare = 50

mapWidth :: (Num a) => a
mapWidth = 800

mapHeight :: (Num a) => a
mapHeight = 600

vel :: (Num a) => a
vel = 3

--MObject: Mobile Objects. Type includes information like position, orientation, etc.
data MObject = Player { xPos :: GLdouble 
                      , yPos :: GLdouble
                      , sightLength :: GLdouble
                      , target :: Maybe MObject
                      , orientation :: Orientation 
                      , reflected :: Bool 
                      } |
               Block { xPos :: GLdouble 
                     , yPos :: GLdouble 
                     , orientation :: Orientation 
                     , reflected :: Bool 
                     } |
               Roller { xPos :: GLdouble
                      , yPos :: GLdouble
                      , orientation :: Orientation
                      , reflected :: Bool
                      , moving :: Maybe Orientation
                      }
            deriving (Eq,Read,Show)

--an object's orientation
data Orientation = North
                 | Northwest
                 | West
                 | Southwest
                 | South
                 | Southeast
                 | East
                 | Northeast
             deriving (Eq,Read,Show)

--clockwise/cclockwise alter an orientation by 45 degrees
-- in the indicated direction
clockwise :: Orientation -> Orientation
clockwise or = case or of
    North -> Northeast
    Northeast -> East
    East -> Southeast
    Southeast -> South
    South -> Southwest
    Southwest -> West
    West -> Northwest
    Northwest -> North

cclockwise :: Orientation -> Orientation
cclockwise or = case or of
    North -> Northwest
    Northwest -> West
    West -> Southwest
    Southwest -> South
    South -> Southeast
    Southeast -> East
    East -> Northeast
    Northeast -> North

--these are used during reflections of objects
clockwise2 :: Orientation -> Orientation
clockwise2 = clockwise . clockwise

cclockwise2 :: Orientation -> Orientation
cclockwise2 = cclockwise . cclockwise

clockwise4 :: Orientation -> Orientation
clockwise4 = clockwise2 . clockwise2

--convert the orientation to a numeric angle
toAngle :: Orientation -> GLdouble
toAngle orientation = case orientation of
    North -> 0
    Northwest -> 45
    West -> 90
    Southwest -> 135
    South -> 180
    Southeast -> 225
    East -> 270
    Northeast -> 315

toReflect :: Bool -> GLdouble
toReflect r = if r then 180 else 0

intersection :: (GLdouble,GLdouble) -> [MObject] -> Bool
intersection p os = isJust $ flip find os (\o -> coord == (case o of
        Block{xPos=x,yPos=y} -> freeToGrid (x,y)
        Roller{xPos=x,yPos=y} -> freeToGrid (x,y)
        otherwise -> error $ "Unsupported MObject constructor: " ++ show o))
    where coord = freeToGrid p

freeToGrid :: (GLdouble,GLdouble) -> (GLint,GLint)
freeToGrid (x,y) = (trunc x, trunc y)
    where trunc z = floor $ z / pixelsPerSquare

gridToFree :: (GLint,GLint) -> (GLdouble,GLdouble)
gridToFree (x,y) = (fl x, fl y)
    where fl z = pixelsPerSquare * (0.5 + fromIntegral z)

--creates a list of squares through which the player's line of sight passes.
-- used for switch detection
viewCheckList :: MObject -> [(GLint,GLint)]
viewCheckList Player{xPos=x,yPos=y,orientation=o,reflected=r} = case o of
    North -> [(x',j) | j <- [y'..h]]
    South -> [(x',j) | j <- [y',(y'-1)..0]]
    East  -> [(i,y') | i <- [x'..w]]
    West  -> [(i,y') | i <- [x',(x'-1)..0]]
    Northeast -> if rx > ry
        then [(x'+i+j,y'+i) | i <- [0..min (h-y') (w-x')], j <- [0,1]]
        else [(x'+i,y'+i+j) | i <- [0..min (h-y') (w-x')], j <- [0,1]] 
    Northwest -> if pixelsPerSquare-rx > ry
        then [(x'-i-j,y'+i) | i <- [0..min (h-y') (x')], j <- [0,1]]
        else [(x'-i,y'+i+j) | i <- [0..min (h-y') (x')], j <- [0,1]] 
    Southeast -> if rx > pixelsPerSquare-ry
        then [(x'+i+j,y'-i) | i <- [0..min (y') (w-x')], j <- [0,1]]
        else [(x'+i,y'-i-j) | i <- [0..min (y') (w-x')], j <- [0,1]] 
    Southwest -> if pixelsPerSquare-rx > pixelsPerSquare-ry
        then [(x'-i-j,y'-i) | i <- [0..min (y') (x')], j <- [0,1]]
        else [(x'-i,y'-i-j) | i <- [0..min (y') (x')], j <- [0,1]] 
    where c = freeToGrid (x,y)
          x' = fst c --the x coordinate on the grid
          y' = snd c --the y coordinate ...
          m = freeToGrid (mapWidth-1,mapHeight-1)
          w = fst m --number of grid elements, width and height
          h = snd m
          rx = rem (floor x) pixelsPerSquare --x relative to the current square
          ry = rem (floor y) pixelsPerSquare --y relative ...
viewCheckList x = error $ "Attempted to find LOS for nonplayer object:\n" ++ show x

findTarget :: [MObject] -> [(GLint,GLint)] -> Maybe MObject
findTarget os (i:is) = case find ((i ==) . getCoords) os of
            Just x -> Just x
            Nothing -> findTarget os is
    where getCoords obj = case obj of
             Block{xPos=x,yPos=y} -> freeToGrid (x,y)
             Roller{xPos=x,yPos=y} -> freeToGrid (x,y)
             otherwise -> error $ "Unsupported MObject constructor: " ++ show obj
findTarget os [] = Nothing
