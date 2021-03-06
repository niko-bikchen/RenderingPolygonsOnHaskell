module P10_TruncatedDodecahedron where

import Graphics.UI.GLUT
import RenderHelper

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (0.0, 0.0, 1.014485),
    (0.3367628, 0.0, 0.9569589),
    (-0.2902233, 0.1708204, 0.9569589),
    (0.1634681, -0.2944272, 0.9569589),
    (0.5914332, 0.1708204, 0.806354),
    (-0.5963465, 0.1527864, 0.806354),
    (-0.4230517, 0.4472136, 0.806354),
    (0.1377417, -0.6, 0.806354),
    (0.8302037, 0.1527864, 0.5626702),
    (0.6667356, 0.4472136, 0.6201961),
    (-0.8014407, -0.0472136, 0.6201961),
    (-0.3477493, 0.7236068, 0.6201961),
    (-0.06735256, -0.8, 0.6201961),
    (0.2694102, -0.8, 0.5626702),
    (0.9618722, -0.0472136, 0.3189863),
    (0.5339072, 0.7236068, 0.4695912),
    (-0.8271671, -0.3527864, 0.4695912),
    (-0.9599955, -0.0763932, 0.3189863),
    (-0.3992021, 0.8763932, 0.3189863),
    (-0.09307895, 0.8944272, 0.4695912),
    (-0.3734757, -0.818034, 0.4695912),
    (0.5081808, -0.818034, 0.3189863),
    (0.9361459, -0.3527864, 0.1683814),
    (1.011448, -0.0763932, -0.0177765),
    (0.4824544, 0.8763932, 0.1683814),
    (0.2436839, 0.8944272, 0.4120653),
    (-0.663699, -0.6472136, 0.4120653),
    (-1.011448, 0.0763932, 0.0177765),
    (-0.5577569, 0.8472136, 0.0177765),
    (-0.5320305, -0.8472136, 0.1683814),
    (0.5577569, -0.8472136, -0.0177765),
    (0.7628511, -0.6472136, 0.1683814),
    (0.9599955, 0.0763932, -0.3189863),
    (0.5320305, 0.8472136, -0.1683814),
    (-0.9618722, 0.0472136, -0.3189863),
    (-0.9361459, 0.3527864, -0.1683814),
    (-0.7628511, 0.6472136, -0.1683814),
    (-0.5081808, 0.818034, -0.3189863),
    (-0.4824544, -0.8763932, -0.1683814),
    (0.3992021, -0.8763932, -0.3189863),
    (0.8014407, 0.0472136, -0.6201961),
    (0.8271671, 0.3527864, -0.4695912),
    (0.663699, 0.6472136, -0.4120653),
    (0.3734757, 0.818034, -0.4695912),
    (-0.8302037, -0.1527864, -0.5626702),
    (-0.2694102, 0.8, -0.5626702),
    (-0.5339072, -0.7236068, -0.4695912),
    (-0.2436839, -0.8944272, -0.4120653),
    (0.09307895, -0.8944272, -0.4695912),
    (0.3477493, -0.7236068, -0.6201961),
    (0.5963465, -0.1527864, -0.806354),
    (0.06735256, 0.8, -0.6201961),
    (-0.6667356, -0.4472136, -0.6201961),
    (-0.5914332, -0.1708204, -0.806354),
    (-0.1377417, 0.6, -0.806354),
    (0.4230517, -0.4472136, -0.806354),
    (0.2902233, -0.1708204, -0.9569589),
    (-0.3367628, 0.0, -0.9569589),
    (-0.1634681, 0.2944272, -0.9569589),
    (0.0, 0.0, -1.014485)
  ]

decagonIndices :: [[Int]]
decagonIndices =
  [ [0, 1, 4, 9, 15, 25, 19, 11, 6, 2],
    [0, 2, 5, 10, 16, 26, 20, 12, 7, 3],
    [1, 3, 7, 13, 21, 31, 22, 14, 8, 4],
    [5, 6, 11, 18, 28, 36, 35, 27, 17, 10],
    [8, 14, 23, 32, 41, 42, 33, 24, 15, 9],
    [12, 20, 29, 38, 47, 48, 39, 30, 21, 13],
    [16, 17, 27, 34, 44, 52, 46, 38, 29, 26],
    [18, 19, 25, 24, 33, 43, 51, 45, 37, 28],
    [22, 31, 30, 39, 49, 55, 50, 40, 32, 23],
    [34, 35, 36, 37, 45, 54, 58, 57, 53, 44],
    [40, 50, 56, 59, 58, 54, 51, 43, 42, 41],
    [46, 52, 53, 57, 59, 56, 55, 49, 48, 47]
  ]

triangleIndices :: [[Int]]
triangleIndices =
  [ [0, 3, 1],
    [2, 6, 5],
    [4, 8, 9],
    [7, 12, 13],
    [10, 17, 16],
    [11, 19, 18],
    [14, 22, 23],
    [15, 24, 25],
    [20, 26, 29],
    [21, 30, 31],
    [27, 35, 34],
    [28, 37, 36],
    [32, 40, 41],
    [33, 42, 43],
    [38, 46, 47],
    [39, 48, 49],
    [44, 53, 52],
    [45, 51, 54],
    [50, 55, 56],
    [57, 58, 59]
  ]

faces :: [PolyFace]
faces =
  [ -- Decagons
    PolyFace (decagonIndices !! 0) red,
    PolyFace (decagonIndices !! 1) yellow,
    PolyFace (decagonIndices !! 2) blue,
    PolyFace (decagonIndices !! 4) orange,
    PolyFace (decagonIndices !! 7) blue,
    PolyFace (decagonIndices !! 3) orange,
    --
    PolyFace (decagonIndices !! 11) orange,
    PolyFace (decagonIndices !! 5) red,
    PolyFace (decagonIndices !! 10) red,
    PolyFace (decagonIndices !! 9) yellow,
    PolyFace (decagonIndices !! 6) blue,
    PolyFace (decagonIndices !! 8) yellow
  ]

renderTruncatedDodecahedronCutaway_1 :: IO ()
renderTruncatedDodecahedronCutaway_1 = do
  renderTruncatedDodecahedron
  polygonMode $= (Line, Line)
  let monochromeFaces = makeSimilarFaces triangleIndices pink
  renderShadowedSpikes monochromeFaces vertices 1.07
  polygonMode $= (Fill, Fill)

renderMonochromeTruncatedDodecahedron :: IO ()
renderMonochromeTruncatedDodecahedron = do
  let decagons = makeSimilarFaces decagonIndices white
  let triangles = makeSimilarFaces triangleIndices white
  renderShadowedPolyFaces decagons vertices
  renderShadowedPolyFaces triangles vertices
  renderPolygonBoundary $ renderTruncatedDodecahedronFrame black

renderTruncatedDodecahedronFrame :: Color3 GLfloat -> IO ()
renderTruncatedDodecahedronFrame color = do
  polygonMode $= (Line, Line)
  let decagons = makeSimilarFaces decagonIndices color
  let triangles = makeSimilarFaces triangleIndices color
  renderShadowedPolyFaces decagons vertices
  renderShadowedPolyFaces triangles vertices
  polygonMode $= (Fill, Fill)

renderTruncatedDodecahedron :: IO ()
renderTruncatedDodecahedron = do
  renderShadowedPolyFaces faces vertices
  let triangles = makeSimilarFaces triangleIndices green
  renderShadowedPolyFaces triangles vertices
  renderPolygonBoundary $ renderTruncatedDodecahedronFrame black
