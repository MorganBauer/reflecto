{-
    GameLogic.hs
    The game logic for the switching game

    © 2011 Stephen Cave
-}

module GameLogic where

import Graphics.UI.GLUT
import GameState


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
toAngle :: Orientation -> GLfloat
toAngle orientation = case orientation of
    North -> 0
    Northeast -> 45
    East -> 90
    Southeast -> 135
    South -> 180
    Southwest -> 225
    West -> 270
    Northwest -> 315

