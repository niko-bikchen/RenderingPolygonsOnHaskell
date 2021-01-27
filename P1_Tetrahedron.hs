module P1_Tetrahedron where

import Graphics.UI.GLUT
import RenderHelper
import Triangle

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (1.0, 1.0, 1.0), -- Vertex A
    (-1.0, 1.0, -1.0), -- Vertex B
    (1.0, -1.0, -1.0), -- Vertex C
    (-1.0, -1.0, 1.0) -- Vertex D
  ]

indices :: [(Int, Int, Int)]
indices =
  [ (3, 1, 2),
    (0, 3, 2),
    (0, 1, 3),
    (0, 2, 1)
  ]

renderTetrahedron :: IO ()
renderTetrahedron = do
  color yellow
  renderTriangleByIndices (indices !! 0) vertices
  color blue
  renderTriangleByIndices (indices !! 1) vertices
  color orange
  renderTriangleByIndices (indices !! 2) vertices
  color red
  renderTriangleByIndices (indices !! 3) vertices