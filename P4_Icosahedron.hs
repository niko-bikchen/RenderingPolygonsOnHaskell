module P4_Icosahedron where

import Graphics.UI.GLUT
import RenderHelper

coordX :: GLfloat
coordX = 0.525731112119133606

coordZ :: GLfloat
coordZ = 0.850650808352039932

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (- coordX, 0.0, coordZ),
    (coordX, 0.0, coordZ),
    (- coordX, 0.0, - coordZ),
    (coordX, 0.0, - coordZ),
    (0.0, coordZ, coordX),
    (0.0, coordZ, - coordX),
    (0.0, - coordZ, coordX),
    (0.0, - coordZ, - coordX),
    (coordZ, coordX, 0.0),
    (- coordZ, coordX, 0.0),
    (coordZ, - coordX, 0.0),
    (- coordZ, - coordX, 0.0)
  ]

indices :: [[Int]]
indices =
  [ [0, 4, 1],
    [0, 9, 4],
    [9, 5, 4],
    [4, 5, 8],
    [4, 8, 1],
    [8, 10, 1],
    [8, 3, 10],
    [5, 3, 8],
    [5, 2, 3],
    [2, 7, 3],
    [7, 10, 3],
    [7, 6, 10],
    [7, 11, 6],
    [11, 0, 6],
    [0, 1, 6],
    [6, 1, 10],
    [9, 0, 11],
    [9, 11, 2],
    [9, 2, 5],
    [7, 2, 11]
  ]

faces :: [PolyFace]
faces =
  [ -- Top "hat"
    PolyFace (indices !! 0) yellow,
    PolyFace (indices !! 1) blue,
    PolyFace (indices !! 2) brown,
    PolyFace (indices !! 3) red,
    PolyFace (indices !! 4) green,
    -- Triangles which come from top
    PolyFace (indices !! 5) brown,
    PolyFace (indices !! 7) blue,
    PolyFace (indices !! 14) red,
    PolyFace (indices !! 16) green,
    PolyFace (indices !! 18) yellow,
    -- Bottom "hat"
    PolyFace (indices !! 19) blue,
    PolyFace (indices !! 9) brown,
    PolyFace (indices !! 10) red,
    PolyFace (indices !! 11) green,
    PolyFace (indices !! 12) yellow,
    -- Triangles which come from bottom
    PolyFace (indices !! 6) yellow,
    PolyFace (indices !! 8) green,
    PolyFace (indices !! 13) brown,
    PolyFace (indices !! 15) blue,
    PolyFace (indices !! 17) red
  ]

renderMonochromeIcosahedron :: IO ()
renderMonochromeIcosahedron = do
  let faces = makeSimilarFaces indices white
  renderShadowedPolyFaces faces vertices

renderIcosahedronFrame :: IO ()
renderIcosahedronFrame = do
  polygonMode $= (Line, Line)
  let faces = makeSimilarFaces indices green
  renderShadowedPolyFaces faces vertices
  polygonMode $= (Fill, Fill)

renderIcosahedron :: IO ()
renderIcosahedron = do
  rotate 120 $ Vector3 (1.0 :: GLfloat) 0.0 0.0
  renderShadowedPolyFaces faces vertices
