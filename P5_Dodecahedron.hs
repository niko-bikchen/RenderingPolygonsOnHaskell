module P5_Dodecahedron where

import Graphics.UI.GLUT
import Pentagon
import RenderHelper

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (1.0, 1.0, 1.0), -- A.0
    (1.0, 1.0, -1.0), -- B.1
    (1.0, -1.0, 1.0), -- C.2
    (1.0, -1.0, -1.0), -- D.3
    (-1.0, 1.0, 1.0), -- E.4
    (-1.0, 1.0, -1.0), -- F.5
    (-1.0, -1.0, 1.0), -- G.6
    (-1.0, -1.0, -1.0), -- H.7
    (0.0, 1.618, 0.618), -- I.8
    (0.0, 1.618, -0.618), -- J.9
    (0.0, -1.618, 0.618), -- K.10
    (0.0, -1.618, -0.618), -- L.11
    (0.618, 0.0, 1.618), -- M.12
    (0.618, 0.0, -1.618), -- N.13
    (-0.618, 0.0, 1.618), -- O.14
    (-0.618, 0.0, -1.618), -- P.15
    (1.618, 0.618, 0.0), -- Q.16
    (1.618, -0.618, 0.0), -- R.17
    (-1.618, 0.618, 0.0), -- S.18
    (-1.618, -0.618, 0.0) -- T.19
  ]

indices :: [(Int, Int, Int, Int, Int)]
indices =
  [ (2, 12, 14, 6, 10),
    (8, 4, 14, 12, 0),
    (19, 6, 14, 4, 18),
    (16, 0, 12, 2, 17),
    (11, 10, 6, 19, 7),
    (3, 17, 2, 10, 11),
    (13, 3, 11, 7, 15),
    (15, 7, 19, 18, 5),
    (1, 16, 17, 3, 13),
    (9, 8, 0, 16, 1),
    (5, 18, 4, 8, 9),
    (15, 5, 9, 1, 13)
  ]

renderDodecahedron :: IO ()
renderDodecahedron = do
  rotate 180 $ Vector3 0.0 (1.0 :: GLfloat) 0.0

  -- Top "hat"
  color red
  renderPentagonByIndices (indices !! 0) vertices
  color yellow
  renderPentagonByIndices (indices !! 1) vertices
  color blue
  renderPentagonByIndices (indices !! 2) vertices
  color brown
  renderPentagonByIndices (indices !! 3) vertices
  renderPentagonByIndices (indices !! 4) vertices
  color blue
  renderPentagonByIndices (indices !! 5) vertices -- Blue between two browns

  -- Bottom "hat"
  color red
  renderPentagonByIndices (indices !! 6) vertices
  color yellow
  renderPentagonByIndices (indices !! 7) vertices
  renderPentagonByIndices (indices !! 8) vertices
  color blue
  renderPentagonByIndices (indices !! 9) vertices
  color red
  renderPentagonByIndices (indices !! 10) vertices
  color brown
  renderPentagonByIndices (indices !! 11) vertices