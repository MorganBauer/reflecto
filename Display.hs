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
    mapM_ renderGObject =<< (get . objects) gstate
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

renderGObject :: GObject -> IO ()
renderGObject o = case o of
    Start{} -> renderStart o
    End{} -> renderEnd o
    Block{} -> renderBlock o
    Roller{} -> renderRoller o
    Wall{} -> renderWall o
    Pit{} -> renderPit o
    otherwise -> error $ "renderGObject: Unsupported GObject constructor " ++ show o

renderStart :: GObject -> IO ()
renderStart Start {xPos=x, yPos=y} = do
    preservingMatrix $ do
        translate (Vector3 x y 0)
        color (Color3 0.6 0 0 :: Color3 GLdouble)
        renderPrimitive Polygon $ mapM_ vertex blockShape
renderStart x = error $ "Attempted to render non-start as a start:\n" ++ show x

renderEnd :: GObject -> IO ()
renderEnd End {xPos=x, yPos=y} = do
    preservingMatrix $ do
        translate (Vector3 x y 0)
        color (Color3 0 0.5 0.5 :: Color3 GLdouble)
        renderPrimitive Polygon $ mapM_ vertex blockShape
renderEnd x = error $ "Attempted to render non-end as an end:\n" ++ show x

renderBlock :: GObject -> IO ()
renderBlock Block {xPos=x, yPos=y, orientation=o, reflected=r} = do
    preservingMatrix $ do
        translate (Vector3 x y 0)
        rotate (toAngle o) (Vector3 0 0 1)
        rotate (toReflect r) (Vector3 0 1 0)
        color (Color3 0.5 0.5 0.5 :: Color3 GLdouble)
        renderPrimitive Polygon $ mapM_ vertex blockShape
renderBlock x = error $ "Attempted to render non-block as a block:\n" ++ show x

blockShape :: [Vertex2 GLdouble]
blockShape = [Vertex2 (-pixelsPerSquare/3) (-pixelsPerSquare/3)
             ,Vertex2 (-pixelsPerSquare/3) ( pixelsPerSquare/3)
             ,Vertex2 ( pixelsPerSquare/3) ( pixelsPerSquare/3)
             ,Vertex2 ( pixelsPerSquare/3) (-pixelsPerSquare/3)
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

--reshape callback, takes care of the window in the event
--  that its shape changes.
reshape :: ReshapeCallback
reshape size@(Size w h) = do
    viewport $= (Position 0 0, size)
    matrixMode $= Projection
    loadIdentity
    ortho2D 0 (fromIntegral w) 0 (fromIntegral h)
    matrixMode $= Modelview 0
