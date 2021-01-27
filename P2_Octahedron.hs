module P2_Octahedron where

import Graphics.UI.GLUT
import RenderHelper
import Triangle

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (1.0, 0.0, 0.0),
    (-1.0, 0.0, 0.0),
    (0.0, 1.0, 0.0),
    (0.0, -1.0, 0.0),
    (0.0, 0.0, 1.0),
    (0.0, 0.0, -1.0)
  ]

indices :: [(Int, Int, Int)]
indices =
  [ (4, 3, 0),
    (4, 0, 2),
    (4, 2, 1),
    (4, 1, 3),
    (0, 3, 5),
    (3, 1, 5),
    (1, 2, 5),
    (2, 0, 5)
  ]

renderOctahedron :: IO ()
renderOctahedron = do
  color yellow
  renderTriangleByIndices (indices !! 0) vertices
  color blue
  renderTriangleByIndices (indices !! 1) vertices
  color brown
  renderTriangleByIndices (indices !! 2) vertices
  color red
  renderTriangleByIndices (indices !! 3) vertices

  renderTriangleByIndices (indices !! 4) vertices
  color yellow
  renderTriangleByIndices (indices !! 5) vertices
  color blue
  renderTriangleByIndices (indices !! 6) vertices
  color brown
  renderTriangleByIndices (indices !! 7) vertices