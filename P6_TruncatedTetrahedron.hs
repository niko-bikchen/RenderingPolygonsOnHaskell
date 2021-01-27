module P6_TruncatedTetrahedron where

import Graphics.UI.GLUT
import Hexagon
import RenderHelper
import Triangle

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (3.0, 1.0, 1.0), -- A.0
    (1.0, 3.0, 1.0), -- B.1
    (1.0, 1.0, 3.0), -- C.2
    (-3.0, -1.0, 1.0), -- D.3
    (-1.0, -3.0, 1.0), -- E.4
    (-1.0, -1.0, 3.0), -- F.5
    (-3.0, 1.0, -1.0), -- G.6
    (-1.0, 3.0, -1.0), -- H.7
    (-1.0, 1.0, -3.0), -- I.8
    (3.0, -1.0, -1.0), -- J.9
    (1.0, -3.0, -1.0), -- K.10
    (1.0, -1.0, -3.0) -- L.11
  ]

hexagonIndices :: [(Int, Int, Int, Int, Int, Int)]
hexagonIndices =
  [ (0, 2, 5, 4, 10, 9),
    (3, 5, 2, 1, 7, 6),
    (10, 4, 3, 6, 8, 11),
    (7, 1, 0, 9, 11, 8)
  ]

triangleIndices :: [(Int, Int, Int)]
triangleIndices =
  [ (1, 2, 0),
    (4, 5, 3),
    (11, 9, 10),
    (8, 6, 7)
  ]

renderTruncatedTetrahedron :: IO ()
renderTruncatedTetrahedron = do
  rotate 120 $ Vector3 0.0 (1.0 :: GLfloat) 0.0

  color yellow
  renderHexagonByIndices (hexagonIndices !! 0) vertices
  color blue
  renderHexagonByIndices (hexagonIndices !! 1) vertices
  color brown
  renderHexagonByIndices (hexagonIndices !! 2) vertices
  color red
  renderHexagonByIndices (hexagonIndices !! 3) vertices

  color brown
  renderTriangleByIndices (triangleIndices !! 0) vertices
  color red
  renderTriangleByIndices (triangleIndices !! 1) vertices
  color blue
  renderTriangleByIndices (triangleIndices !! 2) vertices
  color yellow
  renderTriangleByIndices (triangleIndices !! 3) vertices