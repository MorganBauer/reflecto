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
    polygonMode $= (Line,Fill)
    drawGrid
    mapM_ renderBlock =<< (get . blocks) gstate
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
renderPlayer :: MObject -> IO ()
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

renderBlock :: MObject -> IO ()
renderBlock Block {xPos=x, yPos=y, orientation=o, reflected=r} = do
    preservingMatrix $ do
        translate (Vector3 x y 0)
        rotate (toAngle o) (Vector3 0 0 1)
        rotate (toReflect r) (Vector3 0 1 0)
        color (Color3 0.5 0.5 0.5 :: Color3 GLdouble)
        renderPrimitive Polygon $ mapM_ vertex blockShape
renderBlock x = error $ "Attempted to render non-block as a block:\n" ++ show x

blockShape :: [Vertex2 GLdouble]
blockShape = [Vertex2 (-pixelsPerSquare/2) (-pixelsPerSquare/2)
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
