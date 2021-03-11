module P24_GreatIcosahedron where

import Graphics.UI.GLUT
import RenderHelper

constant :: GLfloat
constant = (sqrt 5 - 1) / 4

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (0.0, -0.5, constant),
    (0.0, -0.5, - constant),
    (0.0, 0.5, constant),
    (0.0, 0.5, - constant),
    (-0.5, constant, 0.0),
    (0.5, constant, 0.0),
    (-0.5, - constant, 0.0),
    (0.5, - constant, 0.0),
    (constant, 0.0, -0.5),
    (constant, 0.0, 0.5),
    (- constant, 0.0, -0.5),
    (- constant, 0.0, 0.5)
  ]

triangleIndices :: [[Int]]
triangleIndices =
  [ [0, 2, 10],
    [0, 10, 5],
    [0, 5, 4],
    [0, 4, 8],
    [0, 8, 2],
    [3, 1, 11],
    [3, 11, 7],
    [3, 7, 6],
    [3, 6, 9],
    [3, 9, 1],
    [2, 6, 7],
    [2, 7, 10],
    [10, 7, 11],
    [10, 11, 5],
    [5, 11, 1],
    [5, 1, 4],
    [4, 1, 9],
    [4, 9, 8],
    [8, 9, 6],
    [8, 6, 2]
  ]

renderGreatIcosahedronFrame :: Color3 GLfloat -> IO ()
renderGreatIcosahedronFrame color = do
  scale 3 (3 :: GLfloat) 3
  polygonMode $= (Line, Line)
  let monochromeFaces = makeSimilarFaces triangleIndices color
  renderShadowedPolyFaces monochromeFaces vertices
  polygonMode $= (Fill, Fill)

renderMonochromeGreatIcosahedron :: IO ()
renderMonochromeGreatIcosahedron = do
  preservingMatrix $ do
    scale 3 (3 :: GLfloat) 3
    let monochromeFaces = makeSimilarFaces triangleIndices white
    renderShadowedPolyFaces monochromeFaces vertices
  renderPolygonBoundary $ renderGreatIcosahedronFrame black

renderGreatIcosahedron :: IO ()
renderGreatIcosahedron = do
  preservingMatrix $ do
    scale 3 (3 :: GLfloat) 3
    let monochromeFaces = makeSimilarFaces triangleIndices cyan
    renderShadowedPolyFaces monochromeFaces vertices
  renderPolygonBoundary $ renderGreatIcosahedronFrame black
