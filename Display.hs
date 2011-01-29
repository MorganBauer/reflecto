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
    renderPlayer (player gstate)
    swapBuffers

drawGrid :: IO ()
drawGrid = do
    color (Color3 0.3 0.3 0 :: Color3 GLfloat)
    renderPrimitive Lines $ mapM_ vertex grid

grid :: [Vertex2 GLfloat]
grid = [Vertex2 x y | x <- [0,pixelsPerSquare..800], y <- [0,600]] ++ 
       [Vertex2 x y | y <- [0,pixelsPerSquare..600], x <- [0,800]]

--draws the player, should happen LAST in display
renderPlayer :: PlayerState -> IO ()
renderPlayer player = do
    x <- (get . xPos) player
    y <- (get . yPos) player
    o <- (get . orientation) player
    preservingMatrix $ do
        translate (Vector3 x y 0)
        rotate (toAngle o) (Vector3 0 0 1)
        color (Color3 0 1 0 :: Color3 GLfloat)
        lineStipple $= Just (1, 0x0F0F)
        renderPrimitive Lines $ do
            vertex (Vertex2 0 0 :: Vertex2 GLfloat)
            vertex (Vertex2 0 500 :: Vertex2 GLfloat)
        lineStipple $= Nothing
        renderPrimitive Polygon $ mapM_ vertex playerShape

--at the moment, an isosceles triangle.
playerShape :: [Vertex2 GLfloat]
playerShape = [Vertex2 0 10
              ,Vertex2 7 (-5)
              ,Vertex2 (-7) (-5)
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
