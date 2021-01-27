module P8_TruncatedCube where

import Graphics.UI.GLUT
import Octagon
import RenderHelper
import Triangle

ksi :: GLfloat
ksi = sqrt 2 - 1

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (ksi, 1, 1), -- A.0
    (ksi, -1, 1), -- B.1
    (ksi, 1, -1), -- C.2
    (ksi, -1, -1), -- D.3
    (- ksi, 1, 1), -- E.4
    (- ksi, -1, 1), -- F.5
    (- ksi, 1, -1), -- G.6
    (- ksi, -1, -1), -- H.7
    --
    (1, ksi, 1), -- I.8
    (1, - ksi, 1), -- J.9
    (1, ksi, -1), -- K.10
    (1, - ksi, -1), -- L.11
    (-1, ksi, 1), -- M.12
    (-1, - ksi, 1), -- N.13
    (-1, ksi, -1), -- O.14
    (-1, - ksi, -1), -- P.15
    --
    (1, 1, ksi), -- Q.16
    (1, -1, ksi), -- R.17
    (1, 1, - ksi), -- S.18
    (1, - 1, - ksi), -- T.19
    (-1, 1, ksi), -- U.20
    (-1, -1, ksi), -- V.21
    (-1, 1, - ksi), -- W.22
    (-1, - 1, - ksi) -- Z.23
  ]

octagonIndices :: [(Int, Int, Int, Int, Int, Int, Int, Int)]
octagonIndices =
  [ (4, 12, 13, 5, 1, 9, 8, 0),
    (15, 23, 21, 13, 12, 20, 22, 14),
    (3, 19, 17, 1, 5, 21, 23, 7),
    (10, 18, 16, 8, 9, 17, 19, 11),
    (6, 22, 20, 4, 0, 16, 18, 2),
    (7, 15, 14, 6, 2, 10, 11, 3)
  ]

triangleIndices :: [(Int, Int, Int)]
triangleIndices =
  [ (17, 9, 1),
    (16, 0, 8),
    (20, 12, 4),
    (21, 5, 13),
    (7, 23, 15),
    (11, 19, 3),
    (2, 18, 10),
    (14, 22, 6)
  ]

renderTruncatedCube :: IO ()
renderTruncatedCube = do
  rotate 120 $ Vector3 0.0 (1.0 :: GLfloat) 0.0

  color yellow
  renderOctagonByIndices (octagonIndices !! 0) vertices
  color blue
  renderOctagonByIndices (octagonIndices !! 1) vertices
  color brown
  renderOctagonByIndices (octagonIndices !! 2) vertices
  color blue
  renderOctagonByIndices (octagonIndices !! 3) vertices
  color brown
  renderOctagonByIndices (octagonIndices !! 4) vertices
  color yellow
  renderOctagonByIndices (octagonIndices !! 5) vertices

  color red
  renderTrianglesByIndices triangleIndices vertices