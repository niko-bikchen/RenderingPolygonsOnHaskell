module P8_TruncatedCube where

import Graphics.UI.GLUT
import RenderHelper

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

octagonIndices :: [[Int]]
octagonIndices =
  [ [4, 12, 13, 5, 1, 9, 8, 0],
    [15, 23, 21, 13, 12, 20, 22, 14],
    [3, 19, 17, 1, 5, 21, 23, 7],
    [10, 18, 16, 8, 9, 17, 19, 11],
    [6, 22, 20, 4, 0, 16, 18, 2],
    [7, 15, 14, 6, 2, 10, 11, 3]
  ]

triangleIndices :: [[Int]]
triangleIndices =
  [ [17, 9, 1],
    [16, 0, 8],
    [20, 12, 4],
    [21, 5, 13],
    [7, 23, 15],
    [11, 19, 3],
    [2, 18, 10],
    [14, 22, 6]
  ]

faces :: [PolyFace]
faces =
  [ -- Octagons
    PolyFace (octagonIndices !! 0) yellow,
    PolyFace (octagonIndices !! 1) blue,
    PolyFace (octagonIndices !! 2) brown,
    PolyFace (octagonIndices !! 3) blue,
    PolyFace (octagonIndices !! 4) brown,
    PolyFace (octagonIndices !! 5) yellow
  ]

renderTruncatedCubeCutaway_1 :: IO ()
renderTruncatedCubeCutaway_1 = do
  renderTruncatedCube
  polygonMode $= (Line, Line)
  let monochromeFaces = makeSimilarFaces triangleIndices green
  renderShadowedSpikes monochromeFaces vertices 1.73
  polygonMode $= (Fill, Fill)

renderMonochromeTruncatedCube :: IO ()
renderMonochromeTruncatedCube = do
  let triangles = makeSimilarFaces triangleIndices white
  let octagons = makeSimilarFaces octagonIndices white
  renderShadowedPolyFaces triangles vertices
  renderShadowedPolyFaces octagons vertices
  renderPolygonBoundary $ renderTruncatedCubeFrame black

renderTruncatedCubeFrame :: Color3 GLfloat -> IO ()
renderTruncatedCubeFrame color = do
  polygonMode $= (Line, Line)
  let triangles = makeSimilarFaces triangleIndices color
  let octagons = makeSimilarFaces octagonIndices color
  renderShadowedPolyFaces triangles vertices
  renderShadowedPolyFaces octagons vertices
  polygonMode $= (Fill, Fill)

renderTruncatedCube :: IO ()
renderTruncatedCube = do
  renderShadowedPolyFaces faces vertices
  let triangles = makeSimilarFaces triangleIndices red
  renderShadowedPolyFaces triangles vertices
  renderPolygonBoundary $ renderTruncatedCubeFrame black
