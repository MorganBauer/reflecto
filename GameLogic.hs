{-
    GameLogic.hs
    The game logic for the switching game

    © 2011 Stephen Cave
-}

module GameLogic where

import Graphics.UI.GLUT hiding (position)
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

--GObject: Game Objects, Objects with various ways of interacting with the player.
data GObject = Player { xPos :: GLdouble 
                      , yPos :: GLdouble
                      , velocity :: Maybe Orientation
                      , sightLength :: GLdouble
                      , target :: Maybe GObject
                      , orientation :: Orientation 
                      , reflected :: Bool 
                      } |
               Start { xPos :: GLdouble
                     , yPos :: GLdouble
                     , orientation :: Orientation
                     } |
               End { xPos :: GLdouble
                   , yPos :: GLdouble
                   } | 
                  --mobile objects
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
                      } |
                  --Stationary objects
               Wall { xPos :: GLdouble
                    , yPos :: GLdouble
                    } |
               Pit { xPos :: GLdouble
                   , yPos :: GLdouble
                   }
            deriving (Eq,Read,Show)

coords :: GObject -> (GLint, GLint)
coords o = case o of
    Player {xPos=x,yPos=y} -> freeToGrid (x,y)
    Start {xPos=x,yPos=y} -> freeToGrid (x,y)
    End {xPos=x,yPos=y} -> freeToGrid (x,y)
    Block {xPos=x,yPos=y} -> freeToGrid (x,y)
    Roller {xPos=x,yPos=y} -> freeToGrid (x,y)
    Wall {xPos=x,yPos=y} -> freeToGrid (x,y)
    Pit {xPos=x,yPos=y} -> freeToGrid (x,y)
    otherwise -> error $ "No implementation for coords for GObject: " ++ show o

position :: GObject -> (GLdouble, GLdouble)
position o = case o of
    Player {xPos=x,yPos=y} -> (x,y)
    Start {xPos=x,yPos=y} -> (x,y)
    End {xPos=x,yPos=y} -> (x,y)
    Block {xPos=x,yPos=y} -> (x,y)
    Roller {xPos=x,yPos=y} -> (x,y)
    Wall {xPos=x,yPos=y} -> (x,y)
    Pit {xPos=x,yPos=y} -> (x,y)
    otherwise -> error $ "No implementation for position for GObject: " ++ show o

--this fixes the input files. Input files use coordinates to specify position
reposition :: GObject -> GObject
reposition ob = let (i,j) = position ob
                    (x,y) = gridToFree (round i, round j)
    in ob{xPos = x, yPos = y}

movep :: GObject -> Bool
movep ob = case ob of
    Player{} -> True
    Start{} -> False
    End{} -> False
    Block{} -> True
    Roller{} -> True
    Wall{} -> False
    Pit{} -> False

pushp :: GObject -> Bool
pushp ob = case ob of
    Player{} -> False
    Start{} -> False
    End{} -> False
    Block{} -> False
    Roller{} -> True
    Wall{} -> False
    Pit{} -> False

moveUpdate :: GObject -> GObject
moveUpdate ob = if pushp ob && isJust (moving ob)
    then let vel' = vel*1.1
             (x,y) = position ob 
             Just o = moving ob
             (x',y') = case o of
                        North -> (x,y+vel')
                        East -> (x+vel',y)
                        South -> (x,y-vel')
                        West -> (x-vel',y)
                        Northeast -> (x+vel',y+vel')
                        Southeast -> (x+vel',y-vel')
                        Southwest -> (x-vel',y-vel')
                        Northwest -> (x-vel',y+vel')
             (i',j') = (gridToFree . freeToGrid) (x',y')
             ob' = ob{ xPos = x', yPos = y'
                     , moving = if vel <= abs (i'-x') || vel <= abs (j'-y')
                            then moving ob else Nothing}
        in ob'
    else ob

move :: GObject -> (GLdouble, GLdouble) -> Orientation -> GObject 
move ob (x,y) or = let (x',y') = (gridToFree . freeToGrid) (x,y) in
  case ob of
    Player{} -> ob {xPos=x',yPos=y',orientation=or}
    Block{} -> ob {xPos=x',yPos=y',orientation=or}
    Roller{} -> ob {xPos=x',yPos=y',orientation=or}
    otherwise -> error $ "No implementation for move for GObject: " ++ show ob

obstructs :: Maybe GObject -> Maybe GObject -> Char -> Bool
obstructs (Just ob) Nothing d = case ob of
    Player{} -> False
    Start{} -> False
    End{} -> False
    Block{} -> True
    Roller{orientation=o} -> d == 'x' && not (o == East || o == West) ||
                             d == 'y' && not (o == North || o == South)
    Wall{} -> True
    Pit{} -> True
obstructs Nothing _ _ = False
obstructs (Just _) (Just _) _ = True

targetp :: GObject -> Bool
targetp ob = case ob of
    Player{} -> False
    Start{} -> False
    End{} -> False
    Block{} -> True
    Roller{} -> True
    Wall{} -> True
    Pit{} -> False

edgeShape :: GObject -> (GLdouble, GLdouble) -> Orientation -> GLdouble 
edgeShape ob (x,y) or = case ob of
    Block{xPos=xc,yPos=yc} -> (if or `elem` [North,South,East,West] 
                            then 1 else (sqrt 2)) *
                    (max (abs $ x - xc) (abs $ y - yc) - pixelsPerSquare/2)
    Roller{xPos=xc,yPos=yc} -> (if or `elem` [North,South,East,West] 
                            then 1 else (sqrt 2)) *
                    (max (abs $ x - xc) (abs $ y - yc) - pixelsPerSquare/2)
    Wall{xPos=xc,yPos=yc} -> (if or `elem` [North,South,East,West]
                            then 1 else (sqrt 2)) *
                    (max (abs $ x - xc) (abs $ y - yc) - pixelsPerSquare/2)
    otherwise -> error $ "No implementation for edgeShape for GObject: " ++ show ob

--an GObject's orientation
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

--these are used during reflections of GObjects
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

intersection :: (GLdouble,GLdouble) -> [GObject] -> Maybe GObject
intersection p os = flip find os (\o -> pc == coords o)
    where pc = freeToGrid p

freeToGrid :: (GLdouble,GLdouble) -> (GLint,GLint)
freeToGrid (x,y) = (trunc x, trunc y)
    where trunc z = floor $ z / pixelsPerSquare

gridToFree :: (GLint,GLint) -> (GLdouble,GLdouble)
gridToFree (x,y) = (fl x, fl y)
    where fl z = pixelsPerSquare * (0.5 + fromIntegral z)

--creates a list of squares through which the player's line of sight passes.
-- used for switch detection
viewCheckList :: GObject -> [(GLint,GLint)]
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
viewCheckList x = error $ "Attempted to find LOS for nonplayer GObject:\n" ++ show x

findTarget :: [GObject] -> [(GLint,GLint)] -> Maybe GObject
findTarget os (i:is) = case find (\x -> targetp x && i == coords x) os of
            Just x -> Just x
            Nothing -> findTarget os is
findTarget os [] = Nothing
