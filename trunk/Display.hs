{-
    SwitchDisplay.hs
    Functions relating to the display callback
    and reshape callback.

    © 2011 Stephen Cave
-}

module Display where

import Graphics.UI.GLUT

import GameState
import GameLogic

--display callback, draws everything
display :: GameState -> DisplayCallback
display gstate = do
    clear [ColorBuffer]
    polygonMode $= (Fill,Fill)
    drawGrid
    mapM_ renderLowGObject =<< (get . objects) gstate
    mapM_ renderMedGObject =<< (get . objects) gstate
    mapM_ renderHighGObject =<< (get . objects) gstate
    renderPlayer =<< (get . player) gstate
    swapBuffers

drawGrid :: IO ()
drawGrid = do
    color (Color3 0.3 0.3 0 :: Color3 GLdouble)
    renderPrimitive Lines $ mapM_ vertex grid

grid :: [Vertex2 GLdouble]
grid = [Vertex2 x y | x <- [0,pixelsPerSquare..mapWidth], y <- [0,mapHeight]] ++ 
       [Vertex2 x y | y <- [0,pixelsPerSquare..mapHeight], x <- [0,mapWidth]]

--draws the player, should happen LAST in display
renderPlayer :: GObject -> IO ()
renderPlayer Player {xPos=x, yPos=y, sightLength=sl, orientation=o, reflected=r} = do
    preservingMatrix $ do
        translate (Vector3 x y 0)
        rotate (toAngle o) (Vector3 0 0 1)
        rotate (toReflect r) (Vector3 0 1 0)
        color (Color3 0 1 0 :: Color3 GLdouble)
        lineStipple $= Just (1, 0x0F0F)
        renderPrimitive Lines $ do
            vertex (Vertex2 0 0 :: Vertex2 GLdouble)
            vertex (Vertex2 0 sl)
        lineStipple $= Nothing
        renderPrimitive Lines $ do
            vertex (Vertex2 (-4) (sl-4))
            vertex (Vertex2 4 (sl+4))
            vertex (Vertex2 4 (sl-4))
            vertex (Vertex2 (-4) (sl+4))
        renderPrimitive Polygon $ mapM_ vertex playerShape
renderPlayer x = error $ "Attempted to render non-player as a player:\n" ++ show x

--at the moment, an isosceles triangle.
playerShape :: [Vertex2 GLdouble]
playerShape = [Vertex2 0 10
              ,Vertex2 7 (-5)
              ,Vertex2 (-7) (-5)
              ]

renderLowGObject :: GObject -> IO ()
renderLowGObject o = case o of
    Start{} -> renderStart o
    End{} -> renderEnd o
    Pit{} -> renderPit o
    Plate{} -> renderPlate o
    _ -> return ()

renderMedGObject :: GObject -> IO ()
renderMedGObject o = case o of
    Beam {} -> renderBeam o
    _ -> return ()

renderHighGObject :: GObject -> IO ()
renderHighGObject o = case o of
    Block{} -> renderBlock o
    Roller{} -> renderRoller o
    Wall{} -> renderWall o
    Door{} -> renderDoor o
    Mirror{} -> renderMirror o
    Diode{} -> renderDiode o
    Sensor{} -> renderSensor o
    _ -> return ()

renderStart :: GObject -> IO ()
renderStart _ = return ()

renderEnd :: GObject -> IO ()
renderEnd End {xPos=x, yPos=y} = do
    preservingMatrix $ do
        translate (Vector3 x y 0)
        color (Color3 0 0.5 0.5 :: Color3 GLdouble)
        lineWidth $= 4
        renderPrimitive Lines $ mapM_ vertex xShape
        lineWidth $= 1
renderEnd x = error $ "Attempted to render non-end as an end:\n" ++ show x

xShape :: [Vertex2 GLdouble]
xShape = [Vertex2 (-pixelsPerSquare/2.3) ( pixelsPerSquare/2.3)
         ,Vertex2 ( pixelsPerSquare/2.3) (-pixelsPerSquare/2.3)
         ,Vertex2 (-pixelsPerSquare/2.3) (-pixelsPerSquare/2.3)
         ,Vertex2 ( pixelsPerSquare/2.3) ( pixelsPerSquare/2.3)
         ]

renderBlock :: GObject -> IO ()
renderBlock Block {xPos=x, yPos=y, orientation=o, reflected=r} = do
    preservingMatrix $ do
        translate (Vector3 x y 0)
        rotate (toAngle o) (Vector3 0 0 1)
        rotate (toReflect r) (Vector3 0 1 0)
        color (Color3 0.5 0.5 0.5 :: Color3 GLdouble)
        renderPrimitive Polygon $ mapM_ vertex blockShape
        color (Color3 0.6 0.6 0.6 :: Color3 GLdouble)
        renderPrimitive Polygon $ mapM_ vertex blockEdge
        rotate (90::GLdouble) (Vector3 0 0 1)
        color (Color3 0.55 0.55 0.55 :: Color3 GLdouble)
        renderPrimitive Polygon $ mapM_ vertex blockEdge
        rotate (90::GLdouble) (Vector3 0 0 1)
        color (Color3 0.4 0.4 0.4 :: Color3 GLdouble)
        renderPrimitive Polygon $ mapM_ vertex blockEdge
        rotate (90::GLdouble) (Vector3 0 0 1)
        color (Color3 0.45 0.45 0.45 :: Color3 GLdouble)
        renderPrimitive Polygon $ mapM_ vertex blockEdge
renderBlock x = error $ "Attempted to render non-block as a block:\n" ++ show x

blockShape :: [Vertex2 GLdouble]
blockShape = [Vertex2 (-pixelsPerSquare/4) (-pixelsPerSquare/4)
             ,Vertex2 (-pixelsPerSquare/4) ( pixelsPerSquare/4)
             ,Vertex2 ( pixelsPerSquare/4) ( pixelsPerSquare/4)
             ,Vertex2 ( pixelsPerSquare/4) (-pixelsPerSquare/4)
             ]

blockEdge :: [Vertex2 GLdouble]
blockEdge = [Vertex2 (-pixelsPerSquare/3) (pixelsPerSquare/3)
            ,Vertex2 ( pixelsPerSquare/3) (pixelsPerSquare/3)
            ,Vertex2 ( pixelsPerSquare/4) (pixelsPerSquare/4)
            ,Vertex2 (-pixelsPerSquare/4) (pixelsPerSquare/4)
            ]

renderRoller :: GObject -> IO ()
renderRoller Roller {xPos=x, yPos=y, orientation=o, reflected=r} = do
    preservingMatrix $ do
        translate (Vector3 x y 0)
        rotate (toAngle o) (Vector3 0 0 1)
        rotate (toReflect r) (Vector3 0 1 0)
        color (Color3 0.7 0.7 0.0 :: Color3 GLdouble)
        renderPrimitive Polygon $ mapM_ vertex rollerShape
renderRoller x = error $ "Attempted to render non-roller as a roller:\n" ++ show x

rollerShape :: [Vertex2 GLdouble]
rollerShape = [Vertex2 (-pixelsPerSquare/2) 0
              ,Vertex2 (-pixelsPerSquare/3) (pixelsPerSquare/3)
              ,Vertex2 ( pixelsPerSquare/3) (pixelsPerSquare/3)
              ,Vertex2 ( pixelsPerSquare/2) 0
              ,Vertex2 ( pixelsPerSquare/3) (-pixelsPerSquare/3)
              ,Vertex2 (-pixelsPerSquare/3) (-pixelsPerSquare/3)
              ]

renderWall Wall{xPos=x,yPos=y} = do
    preservingMatrix $ do
        translate (Vector3 x y 0)
        color (Color3 0.8 0.8 0.8 :: Color3 GLdouble)
        renderPrimitive Polygon $ mapM_ vertex wallShape
renderWall x = error $ "Attempted to render non-wall as a wall:\n" ++ show x

wallShape :: [Vertex2 GLdouble]
wallShape =  [Vertex2 (-pixelsPerSquare/2) (-pixelsPerSquare/2)
             ,Vertex2 (-pixelsPerSquare/2) ( pixelsPerSquare/2)
             ,Vertex2 ( pixelsPerSquare/2) ( pixelsPerSquare/2)
             ,Vertex2 ( pixelsPerSquare/2) (-pixelsPerSquare/2)
             ]

renderPit Pit{xPos=x,yPos=y} = do
    preservingMatrix $ do
        translate (Vector3 x y 0)
        color (Color3 0 0 0 :: Color3 GLdouble)
        renderPrimitive Polygon $ mapM_ vertex pitShape
renderPit x = error $ "Attempted to render non-pit as a pit:\n" ++ show x

pitShape :: [Vertex2 GLdouble]
pitShape =  [Vertex2 (-pixelsPerSquare/2) (-pixelsPerSquare/2)
            ,Vertex2 (-pixelsPerSquare/2) ( pixelsPerSquare/2)
            ,Vertex2 ( pixelsPerSquare/2) ( pixelsPerSquare/2)
            ,Vertex2 ( pixelsPerSquare/2) (-pixelsPerSquare/2)
            ]

renderDoor Door{xPos=x,yPos=y,orientation=o,closed=c} = do
    preservingMatrix $ do
        translate (Vector3 x y 0)
        rotate (toAngle o) (Vector3 0 0 1)
        color (Color3 0.2 0.2 0.2 :: Color3 GLdouble)
        lineWidth $= 4
        renderPrimitive Lines $ mapM_ vertex (if c then doorClosedShape else doorOpenShape)
        lineWidth $= 1
renderDoor x = error $ "Attempted to render non-door as a door:\n" ++ show x

doorClosedShape :: [Vertex2 GLdouble]
doorClosedShape = [Vertex2 (-pixelsPerSquare/2) 0
                  ,Vertex2 ( pixelsPerSquare/2) 0
                  ]

doorOpenShape :: [Vertex2 GLdouble]
doorOpenShape = [Vertex2 (-pixelsPerSquare/2) 0
                  ,Vertex2 (-pixelsPerSquare/2) (-pixelsPerSquare/2)
                  ,Vertex2 ( pixelsPerSquare/2) (-pixelsPerSquare/2)
                  ,Vertex2 ( pixelsPerSquare/2) 0
                  ]

renderPlate Plate{xPos=x,yPos=y,active=a} = do
    preservingMatrix $ do
        translate (Vector3 x y 0)
        if a 
          then color (Color3 0   0.6 0 :: Color3 GLdouble)
          else color (Color3 0.5 0.3 0 :: Color3 GLdouble)
        renderPrimitive Polygon $ mapM_ vertex plateShape

plateShape :: [Vertex2 GLdouble]
plateShape = [Vertex2 (-pixelsPerSquare/2) (-pixelsPerSquare/2)
             ,Vertex2 (-pixelsPerSquare/2) ( pixelsPerSquare/2)
             ,Vertex2 ( pixelsPerSquare/2) ( pixelsPerSquare/2)
             ,Vertex2 ( pixelsPerSquare/2) (-pixelsPerSquare/2)
             ]

renderMirror Mirror{xPos=x,yPos=y,orientation=o,reflected=r} = do
    preservingMatrix $ do
        translate (Vector3 x y 0)
        rotate (toAngle o) (Vector3 0 0 1)
        rotate (toReflect r) (Vector3 0 1 0)
        color (Color3 0 0 0 :: Color3 GLdouble)
        renderPrimitive Polygon $ mapM_ vertex backShape
        color (Color3 0.2 0.4 1.0 :: Color3 GLdouble)
        lineWidth $= 4
        renderPrimitive Lines $ mapM_ vertex faceShape
        lineWidth $= 1
    where backShape = if o `elem` [North,East,South,West] then mirrorBackSt else mirrorBackDi
          faceShape = if o `elem` [North,East,South,West] then lineSt else lineDi

mirrorBackSt :: [Vertex2 GLdouble]
mirrorBackSt = [Vertex2 (-pixelsPerSquare/2) 0
               ,Vertex2 ( pixelsPerSquare/2) 0
               ,Vertex2 ( pixelsPerSquare/2) (-pixelsPerSquare/2)
               ,Vertex2 (-pixelsPerSquare/2) (-pixelsPerSquare/2)
               ]
mirrorBackDi :: [Vertex2 GLdouble]
mirrorBackDi = [Vertex2 (-pixelsPerSquare/sqrt 2) 0
               ,Vertex2 ( pixelsPerSquare/sqrt 2) 0
               ,Vertex2 0 (-pixelsPerSquare/sqrt 2)
               ]
lineSt :: [Vertex2 GLdouble]
lineSt = [Vertex2 (-pixelsPerSquare/2) 0
         ,Vertex2 ( pixelsPerSquare/2) 0
         ]
lineDi :: [Vertex2 GLdouble]
lineDi = [Vertex2 (-pixelsPerSquare/sqrt 2) 0
         ,Vertex2 ( pixelsPerSquare/sqrt 2) 0
         ]

renderBeam Beam{xPos=x,yPos=y,orientation=o,sightLength=sl} = do
    preservingMatrix $ do
        translate (Vector3 x y 0)
        rotate (toAngle o) (Vector3 0 0 1)
        color (Color3 1.0 0.7 0 :: Color3 GLdouble)
        renderPrimitive Lines $ do
            vertex (Vertex2 0 0 :: Vertex2 GLdouble)
            vertex (Vertex2 0 (sl+extend))
    where beamShape = if o `elem` [North,East,South,West] then lineSt else lineDi
          extend = pixelsPerSquare / if o `elem` [North,East,South,West] then 2 else sqrt 2

renderDiode Diode{xPos=x,yPos=y,orientation=o} = do
    preservingMatrix $ do
        translate (Vector3 x y 0)
        rotate (toAngle o) (Vector3 0 0 1)
        color (Color3 0.1 0.1 0.1 :: Color3 GLdouble)
        renderPrimitive Polygon $ mapM_ vertex diodeShape
        color (Color3 1.0 0.7 0 :: Color3 GLdouble)
        renderPrimitive Lines $ do
            vertex (Vertex2 0 0 :: Vertex2 GLdouble)
            vertex (Vertex2 0 (pixelsPerSquare / sqrt 2)::Vertex2 GLdouble)

diodeShape :: [Vertex2 GLdouble]
diodeShape = [Vertex2 (-pixelsPerSquare/2) ( pixelsPerSquare*e)
             ,Vertex2 (-pixelsPerSquare/2) (-pixelsPerSquare*e)
             ,Vertex2 (-pixelsPerSquare*e) (-pixelsPerSquare/2)
             ,Vertex2 ( pixelsPerSquare*e) (-pixelsPerSquare/2)
             ,Vertex2 ( pixelsPerSquare/2) (-pixelsPerSquare*e)
             ,Vertex2 ( pixelsPerSquare/2) ( pixelsPerSquare*e)
             ,Vertex2 ( pixelsPerSquare*e) ( pixelsPerSquare/2)
             ,Vertex2 (-pixelsPerSquare*e) ( pixelsPerSquare/2)
             ]
    where e = 0.5*(sqrt 2 - 1)

renderSensor Sensor{xPos=x,yPos=y,active=a} = do
    preservingMatrix $ do
        translate (Vector3 x y 0)
        scale 0.6 0.6 (1.0 :: GLdouble)
        if a then color (Color3 1 0.7 0.0 :: Color3 GLdouble)
             else color (Color3 0.1 0.1 0.1 :: Color3 GLdouble)
        renderPrimitive Polygon $ mapM_ vertex diodeShape

--reshape callback, takes care of the window in the event
--  that its shape changes.
reshape :: ReshapeCallback
reshape size@(Size w h) = do
    viewport $= (Position 0 0, size)
    matrixMode $= Projection
    loadIdentity
    ortho2D 0 (fromIntegral w) 0 (fromIntegral h)
    matrixMode $= Modelview 0
