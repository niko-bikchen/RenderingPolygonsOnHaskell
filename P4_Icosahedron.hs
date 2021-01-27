module P4_Icosahedron where

import Graphics.UI.GLUT
import RenderHelper
import Triangle

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

indices :: [(Int, Int, Int)]
indices =
  [ (0, 4, 1),
    (0, 9, 4),
    (9, 5, 4),
    (4, 5, 8),
    (4, 8, 1),
    (8, 10, 1),
    (8, 3, 10),
    (5, 3, 8),
    (5, 2, 3),
    (2, 7, 3),
    (7, 10, 3),
    (7, 6, 10),
    (7, 11, 6),
    (11, 0, 6),
    (0, 1, 6),
    (6, 1, 10),
    (9, 0, 11),
    (9, 11, 2),
    (9, 2, 5),
    (7, 2, 11)
  ]

renderIcosahedron :: IO ()
renderIcosahedron = do
  
  rotate 120 $ Vector3 (1.0 :: GLfloat) 0.0 0.0

  -- Top "hat"
  color yellow
  renderTriangleByIndices (indices !! 0) vertices
  color blue
  renderTriangleByIndices (indices !! 1) vertices
  color brown
  renderTriangleByIndices (indices !! 2) vertices
  color red
  renderTriangleByIndices (indices !! 3) vertices
  color green
  renderTriangleByIndices (indices !! 4) vertices

  -- Triangles which come from top
  color brown
  renderTriangleByIndices (indices !! 5) vertices
  color blue
  renderTriangleByIndices (indices !! 7) vertices
  color red
  renderTriangleByIndices (indices !! 14) vertices
  color green
  renderTriangleByIndices (indices !! 16) vertices
  color yellow
  renderTriangleByIndices (indices !! 18) vertices

  -- Bottom "hat"
  color blue
  renderTriangleByIndices (indices !! 19) vertices
  color brown
  renderTriangleByIndices (indices !! 9) vertices
  color red
  renderTriangleByIndices (indices !! 10) vertices
  color green
  renderTriangleByIndices (indices !! 11) vertices
  color yellow
  renderTriangleByIndices (indices !! 12) vertices

  -- Triangles which come from bottom
  renderTriangleByIndices (indices !! 6) vertices
  color green
  renderTriangleByIndices (indices !! 8) vertices
  color brown
  renderTriangleByIndices (indices !! 13) vertices
  color blue
  renderTriangleByIndices (indices !! 15) vertices
  color red
  renderTriangleByIndices (indices !! 17) vertices