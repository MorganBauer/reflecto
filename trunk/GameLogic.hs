{-
    GameLogic.hs
    The game logic for the switching game

    © 2011 Stephen Cave
-}

module GameLogic where

import Graphics.UI.GLUT (GLdouble,GLint)
import Data.List (find,partition)
import Data.Maybe (fromJust,isJust)

pixelsPerSquare :: (Num a) => a
pixelsPerSquare = 50

mapWidth :: (Num a) => a
mapWidth = 800

mapHeight :: (Num a) => a
mapHeight = 600

vel :: (Num a) => a
vel = 3

data Source = PlayerSource | LaserSource
    deriving (Eq,Show,Read)

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
               Mirror  { xPos :: GLdouble
                      , yPos :: GLdouble
                      , orientation :: Orientation
                      , reflected :: Bool
                      } |
               Beam { xPos :: GLdouble
                    , yPos :: GLdouble
                    , orientation :: Orientation
                    , sightLength :: GLdouble
                    , target :: Maybe GObject
                    , source :: Source
                    } |
                  --Stationary objects
               Diode { xPos :: GLdouble
                      , yPos :: GLdouble
                      , orientation :: Orientation
                      } |
               Sensor { xPos :: GLdouble
                      , yPos :: GLdouble
                      , active :: Bool
                      } |
               Wall { xPos :: GLdouble
                    , yPos :: GLdouble
                    } |
               Pit { xPos :: GLdouble
                   , yPos :: GLdouble
                   } |
               Door { xPos :: GLdouble
                    , yPos :: GLdouble
                    , orientation :: Orientation
                    , defaultClosed :: Bool
                    , closed :: Bool
                    , triggers :: [(GLint,GLint)]
                    } |
               Plate { xPos :: GLdouble
                     , yPos :: GLdouble
                     , active :: Bool
                     } 
            deriving (Eq,Read,Show)


coords :: GObject -> (GLint, GLint)
coords o = freeToGrid (xPos o, yPos o)

position :: GObject -> (GLdouble, GLdouble)
position o = (xPos o, yPos o)

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
    Door{} -> False
    Plate{} -> False
    Mirror{} -> True
    Beam{} -> False
    Diode{} -> False
    Sensor{} -> False

pushp :: GObject -> Bool
pushp ob = case ob of
    Roller{} -> True
    _ -> False

limitedVel :: GObject -> Maybe Orientation -> Maybe Orientation
limitedVel ob (Just vel) = case ob of
    Roller{orientation=or} -> project vel or

project :: Orientation -> Orientation -> Maybe Orientation
project o1 o2 | o2 == o1 || clockwise o2 == o1 || cclockwise o2 == o1 = Just o2
              | cclockwise (cclockwise2 o2) == o1 || clockwise (clockwise2 o2) == o1 || clockwise4 o2 == o1 = Just $ clockwise4 o2
              | clockwise2 o2 == o1 || cclockwise2 o2 == o1 = Nothing

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

move :: GObject -> (GLdouble, GLdouble) -> Orientation -> Bool -> GObject 
move ob (x,y) or ref = let (x',y') = (gridToFree . freeToGrid) (x,y) in
  case ob of
    Player{} -> ob {xPos=x',yPos=y',orientation=or,reflected=ref}
    Block{} -> ob {xPos=x',yPos=y',orientation=or,reflected=ref}
    Roller{} -> ob {xPos=x',yPos=y',orientation=or,reflected=ref}
    Mirror{} -> ob {xPos=x',yPos=y',orientation=or,reflected=ref}
    _ -> error $ "No implementation for move for GObject: " ++ show ob

obstructs :: [GObject] -> [GObject] -> Char -> Bool
obstructs [] _ _ = False
obstructs (ob:obs) k d = case ob of
    Block{} -> True
    Roller{orientation=o} -> any (not . coverable) k || 
                d == 'x' && not (o == East || o == West) ||
                d == 'y' && not (o == North || o == South) || 
                obstructs obs k d
    Wall{} -> True
    Pit{} -> True
    Door{closed=c} -> c || obstructs obs k d
    Mirror{} -> True
    Diode{} -> True
    Sensor{} -> True
    _ -> obstructs obs k d

coverable o = case o of
    Player{} -> True
    Start{} -> True
    End{} -> True
    Block{} -> False
    Roller{} -> False
    Wall{} -> False
    Pit{} -> False
    Door{closed=c} -> not c
    Plate{} -> True
    Mirror{} -> False
    Beam{} -> True
    Diode{} -> False
    Sensor{} -> False

targetp :: GObject -> Bool
targetp ob = case ob of
    Player{} -> False
    Start{} -> False
    End{} -> False
    Block{} -> True
    Roller{} -> True
    Wall{} -> True
    Pit{} -> False
    Door{closed=c} -> c
    Plate{} -> False
    Mirror{} -> True
    Beam{} -> False
    Diode{} -> True
    Sensor{} -> True

doorUpdate os = map (doorUpdate' os) os

doorUpdate' os o = case o of
    Door {defaultClosed=dfs,triggers=ts} -> o{closed = checkTriggers os' dfs}
        where os' = concatMap (flip intersection os) ts --all objects that exist at the point of the trigger
    _ -> o

checkTriggers :: [GObject] -> Bool -> Bool
checkTriggers os = if any isActive os then not else id

isActive o = if hasActivation o then active o else False

hasActivation o = case o of
    Plate{} -> True
    Sensor{} -> True
    _ -> False

activeUpdate p os = map (activeUpdate' (p:os)) os

activeUpdate' os o = case o of
    Plate{} -> o{active = not $ null $ filter (/= o) $ intersection (coords o) os}
    Sensor{} -> o{active = not $ null $ filter (/= o) $ intersection (coords o) os}
    _ -> o

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
    Door{xPos=xc,yPos=yc} -> (if or `elem` [North,South,East,West]
                            then 1 else (sqrt 2)) *
                    (max (abs $ x - xc) (abs $ y - yc) - pixelsPerSquare/2)
    Mirror{xPos=xc,yPos=yc} -> (if or `elem` [North,South,East,West]
                            then 1 else (sqrt 2)) *
                    (max (abs $ x - xc) (abs $ y - yc) - pixelsPerSquare/2)
    Diode{xPos=xc,yPos=yc} -> (if or `elem` [North,South,East,West]
                            then 1 else (sqrt 2)) *
                    (max (abs $ x - xc) (abs $ y - yc) - pixelsPerSquare/2)
    Sensor{xPos=xc,yPos=yc} -> (if or `elem` [North,South,East,West]
                            then 1 else (sqrt 2)) *
                    (max (abs $ x - xc) (abs $ y - yc) - pixelsPerSquare/2)
    _ -> error $ "No implementation for edgeShape for GObject: " ++ show ob

--a GObject's orientation
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

intersection :: (GLint,GLint) -> [GObject] -> [GObject]
intersection p os = flip filter os (\o -> p == coords o)

freeToGrid :: (GLdouble,GLdouble) -> (GLint,GLint)
freeToGrid (x,y) = (trunc x, trunc y)
    where trunc z = floor $ z / pixelsPerSquare

gridToFree :: (GLint,GLint) -> (GLdouble,GLdouble)
gridToFree (x,y) = (fl x, fl y)
    where fl z = pixelsPerSquare * (0.5 + fromIntegral z)

--creates a list of squares through which the object's line of sight passes.
-- used for beam termination and redirection
viewCheckList :: GObject -> [(GLint,GLint)]
viewCheckList obj = case o of
    North -> [(x',j) | j <- [(y'+1)..h]]
    South -> [(x',j) | j <- [(y'-1),(y'-2)..0]]
    East  -> [(i,y') | i <- [(x'+1)..w]]
    West  -> [(i,y') | i <- [(x'-1),(x'-2)..0]]
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
    where x = xPos obj
          y = yPos obj
          o = orientation obj
          c = freeToGrid (x,y)
          x' = fst c --the x coordinate on the grid
          y' = snd c --the y coordinate ...
          m = freeToGrid (mapWidth-1,mapHeight-1)
          w = fst m --number of grid elements, width and height
          h = snd m
          rx = rem (floor x) pixelsPerSquare --x relative to the current square
          ry = rem (floor y) pixelsPerSquare --y relative ...
--special case: object is exactly centered
viewCheckList' :: GObject -> [(GLint,GLint)]
viewCheckList' obj = case o of
    North -> [(x',j) | j <- [(y'+1)..h]]
    South -> [(x',j) | j <- [(y'-1),(y'-2)..0]]
    East  -> [(i,y') | i <- [(x'+1)..w]]
    West  -> [(i,y') | i <- [(x'-1),(x'-2)..0]]
    Northeast -> [(x'+i,y'+i) | i <- [1..min (h-y') (w-x')]]
    Northwest -> [(x'-i,y'+i) | i <- [1..min (h-y') (x')]]
    Southeast -> [(x'+i,y'-i) | i <- [1..min (y') (w-x')]]
    Southwest -> [(x'-i,y'-i) | i <- [1..min (y') (x')]]
    where (x,y) = position obj
          (x',y') = coords obj
          o = orientation obj
          m = freeToGrid (mapWidth-1,mapHeight-1)
          w = fst m --number of grid elements, width and height
          h = snd m

findTarget :: [GObject] -> [(GLint,GLint)] -> Maybe GObject
findTarget os (i:is) = case find (\x -> targetp x && i == coords x) os of
            Just x -> Just x
            Nothing -> findTarget os is
findTarget os [] = Nothing

makeBeams [] = []
makeBeams (o:os) = case o of
    Diode{xPos=x,yPos=y,orientation=od} -> o : 
            Beam{xPos=x,yPos=y,orientation=od, sightLength=1000
                ,target=Nothing,source=LaserSource} : makeBeams os
    _ -> o : makeBeams os

beamExtend os = map (updateBeam os') bs' ++ os'
    where (bs,os') = partition isBeam os
          bs' = reflectedBeams bs os'

isBeam o = case o of
            Beam{} -> True
            _      -> False

updateBeam os b = case b of
    Beam{xPos=x,yPos=y,orientation=o} -> b{target=t,sightLength=getSL}
        where t = findTarget os (viewCheckList' b)
              getSL = case t of
                Nothing -> 1000
                Just ob -> edgeShape ob (x,y) o
    _ -> b

reflectedBeams [] _ = []
reflectedBeams (b:bs) os = case t of
        Just Mirror{xPos=x,yPos=y,orientation=mo} -> 
            if clockwise4 mo == cclockwise bo 
            then b' : reflectedBeams (updateBeam os b'{xPos=x,yPos=y,orientation=(clockwise2 bo)} : bs) os
            else if clockwise4 mo == clockwise bo
            then b' : reflectedBeams (updateBeam os b'{xPos=x,yPos=y,orientation=(cclockwise2 bo)} : bs) os
            else b' : reflectedBeams bs os
        Just Sensor{xPos=x,yPos=y} -> b' : reflectedBeams (updateBeam os b'{xPos=x,yPos=y} : bs) os
        _ -> b' : reflectedBeams bs os
    where b' = updateBeam os b
          t = target b'
          bo = orientation b'
