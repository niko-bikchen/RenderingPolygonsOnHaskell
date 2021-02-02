module P5_Dodecahedron where

import Graphics.UI.GLUT
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

indices :: [[Int]]
indices =
  [ [2, 12, 14, 6, 10],
    [8, 4, 14, 12, 0],
    [19, 6, 14, 4, 18],
    [16, 0, 12, 2, 17],
    [11, 10, 6, 19, 7],
    [3, 17, 2, 10, 11],
    [13, 3, 11, 7, 15],
    [15, 7, 19, 18, 5],
    [1, 16, 17, 3, 13],
    [9, 8, 0, 16, 1],
    [5, 18, 4, 8, 9],
    [15, 5, 9, 1, 13]
  ]

faces :: [PolyFace]
faces =
  [ -- Top "hat"
    PolyFace (indices !! 0) red,
    PolyFace (indices !! 1) yellow,
    PolyFace (indices !! 2) blue,
    PolyFace (indices !! 3) brown,
    PolyFace (indices !! 4) brown,
    PolyFace (indices !! 5) blue, -- Blue between two browns
    -- Bottom "hat"
    PolyFace (indices !! 6) red,
    PolyFace (indices !! 7) yellow,
    PolyFace (indices !! 8) yellow,
    PolyFace (indices !! 9) blue,
    PolyFace (indices !! 10) red,
    PolyFace (indices !! 11) brown
  ]

renderDodecahedron :: IO ()
renderDodecahedron = do
  rotate 180 $ Vector3 0.0 (1.0 :: GLfloat) 0.0
  renderShadowedPolyFaces faces vertices