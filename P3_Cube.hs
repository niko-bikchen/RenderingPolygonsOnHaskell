module P3_Cube where

import Graphics.UI.GLUT
import RenderHelper
import Square

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (1.0, 1.0, -1.0),
    (-1.0, 1.0, -1.0),
    (-1.0, 1.0, 1.0),
    (1.0, 1.0, 1.0),
    (1.0, -1.0, 1.0),
    (-1.0, -1.0, 1.0),
    (-1.0, -1.0, -1.0),
    (1.0, -1.0, -1.0)
  ]

indices :: [(Int, Int, Int, Int)]
indices =
  [ (0, 1, 2, 3),
    (4, 5, 6, 7),
    (3, 2, 5, 4),
    (7, 6, 1, 0),
    (2, 1, 6, 5),
    (0, 3, 4, 7)
  ]

renderCube :: IO ()
renderCube = do
  color yellow
  renderSquareByIndices (indices !! 0) vertices
  renderSquareByIndices (indices !! 1) vertices
  color blue
  renderSquareByIndices (indices !! 2) vertices
  renderSquareByIndices (indices !! 3) vertices
  color brown
  renderSquareByIndices (indices !! 4) vertices
  renderSquareByIndices (indices !! 5) vertices