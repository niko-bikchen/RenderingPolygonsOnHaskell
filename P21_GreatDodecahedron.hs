module P21_GreatDodecahedron where

import Graphics.UI.GLUT
import RenderHelper

constant :: GLfloat
constant = (1 + sqrt 5) / 4

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (0.5, 0.0, constant),
    (0.5, 0.0, - constant),
    (-0.5, 0.0, constant),
    (-0.5, 0.0, - constant),
    (constant, 0.5, 0.0),
    (constant, -0.5, 0.0),
    (- constant, 0.5, 0.0),
    (- constant, -0.5, 0.0),
    (0.0, constant, 0.5),
    (0.0, constant, -0.5),
    (0.0, - constant, 0.5),
    (0.0, - constant, -0.5)
  ]

pentagonIndices :: [[Int]]
pentagonIndices =
  [ [0, 2, 7, 11, 5],
    [0, 5, 1, 9, 8],
    [0, 8, 6, 7, 10],
    [1, 3, 6, 8, 4],
    [1, 4, 0, 10, 11],
    [1, 11, 7, 6, 9],
    [2, 0, 4, 9, 6],
    [2, 6, 3, 11, 10],
    [2, 10, 5, 4, 8],
    [3, 1, 5, 10, 7],
    [3, 7, 2, 8, 9],
    [3, 9, 4, 5, 11]
  ]

faces :: [PolyFace]
faces =
  [ PolyFace (pentagonIndices !! 0) yellow,
    PolyFace (pentagonIndices !! 1) blue,
    PolyFace (pentagonIndices !! 2) red,
    PolyFace (pentagonIndices !! 3) yellow,
    PolyFace (pentagonIndices !! 4) green,
    PolyFace (pentagonIndices !! 5) brown,
    PolyFace (pentagonIndices !! 6) pink,
    PolyFace (pentagonIndices !! 7) blue,
    PolyFace (pentagonIndices !! 8) brown,
    PolyFace (pentagonIndices !! 9) pink,
    PolyFace (pentagonIndices !! 10) green,
    PolyFace (pentagonIndices !! 11) red
  ]

renderMonochromeGreatDodecahedron :: IO ()
renderMonochromeGreatDodecahedron = do
  preservingMatrix $ do
    scale 1.5 (1.5 :: GLfloat) 1.5
    let monochromeFaces = makeSimilarFaces pentagonIndices white
    renderShadowedPolyFaces monochromeFaces vertices
  renderPolygonBoundary $ renderGreatDodecahedronFrame black

renderGreatDodecahedronFrame :: Color3 GLfloat -> IO ()
renderGreatDodecahedronFrame color = do
  scale 1.5 (1.5 :: GLfloat) 1.5
  polygonMode $= (Line, Line)
  let monochromeFaces = makeSimilarFaces pentagonIndices color
  renderShadowedPolyFaces monochromeFaces vertices
  polygonMode $= (Fill, Fill)

renderGreatDodecahedron :: IO ()
renderGreatDodecahedron = do
  preservingMatrix $ do
    scale 1.5 (1.5 :: GLfloat) 1.5
    renderShadowedPolyFaces faces vertices
  renderPolygonBoundary $ renderGreatDodecahedronFrame black
