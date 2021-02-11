module P14_Rhombicosidodecahedron where

import Graphics.UI.GLUT
import RenderHelper

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (0.0, 0.0, 1.026054),
    (0.447838, 0.0, 0.9231617),
    (-0.02363976, 0.4472136, 0.9231617),
    (-0.4050732, 0.190983, 0.9231617),
    (-0.1693344, -0.4145898, 0.9231617),
    (0.4241982, 0.4472136, 0.8202696),
    (0.7673818, 0.190983, 0.6537868),
    (0.5552827, -0.4145898, 0.7566788),
    (-0.2312241, 0.7562306, 0.6537868),
    (-0.5744076, -0.2236068, 0.8202696),
    (-0.6126576, 0.5, 0.6537868),
    (0.1738492, -0.6708204, 0.7566788),
    (-0.4669629, -0.6381966, 0.6537868),
    (0.493393, 0.7562306, 0.4873039),
    (0.8748265, -0.2236068, 0.4873039),
    (0.8365765, 0.5, 0.320821),
    (0.7054921, -0.6381966, 0.3844118),
    (0.08831973, 0.9472136, 0.3844118),
    (-0.5434628, 0.809017, 0.320821),
    (-0.8866463, -0.1708204, 0.4873039),
    (-0.9102861, 0.2763932, 0.3844118),
    (-0.1237794, -0.8944272, 0.4873039),
    (0.3240586, -0.8944272, 0.3844118),
    (-0.7792016, -0.5854102, 0.320821),
    (0.6289922, 0.809017, 0.05144604),
    (1.010426, -0.1708204, 0.05144604),
    (0.9867859, 0.2763932, -0.05144604),
    (0.8410913, -0.5854102, -0.05144604),
    (-0.223919, 1.0, 0.05144604),
    (0.223919, 1.0, -0.05144604),
    (-0.8410913, 0.5854102, 0.05144604),
    (-0.9867859, -0.2763932, 0.05144604),
    (-1.010426, 0.1708204, -0.05144604),
    (-0.223919, -1.0, 0.05144604),
    (0.223919, -1.0, -0.05144604),
    (-0.6289922, -0.809017, -0.05144604),
    (0.7792016, 0.5854102, -0.320821),
    (0.9102861, -0.2763932, -0.3844118),
    (0.8866463, 0.1708204, -0.4873039),
    (0.5434628, -0.809017, -0.320821),
    (-0.3240586, 0.8944272, -0.3844118),
    (0.1237794, 0.8944272, -0.4873039),
    (-0.7054921, 0.6381966, -0.3844118),
    (-0.8365765, -0.5, -0.320821),
    (-0.8748265, 0.2236068, -0.4873039),
    (-0.08831973, -0.9472136, -0.3844118),
    (-0.493393, -0.7562306, -0.4873039),
    (0.4669629, 0.6381966, -0.6537868),
    (0.6126576, -0.5, -0.6537868),
    (0.5744076, 0.2236068, -0.8202696),
    (0.2312241, -0.7562306, -0.6537868),
    (-0.1738492, 0.6708204, -0.7566788),
    (-0.5552827, 0.4145898, -0.7566788),
    (-0.7673818, -0.190983, -0.6537868),
    (-0.4241982, -0.4472136, -0.8202696),
    (0.1693344, 0.4145898, -0.9231617),
    (0.4050732, -0.190983, -0.9231617),
    (0.02363976, -0.4472136, -0.9231617),
    (-0.447838, 0.0, -0.9231617),
    (0.0, 0.0, -1.026054)
  ]

squareIndices :: [[Int]]
squareIndices =
  [ [0, 1, 5, 2],
    [0, 3, 9, 4],
    [1, 7, 14, 6],
    [2, 8, 10, 3],
    [4, 12, 21, 11],
    [5, 6, 15, 13],
    [7, 11, 22, 16],
    [8, 17, 28, 18],
    [9, 19, 23, 12],
    [10, 18, 30, 20],
    [13, 24, 29, 17],
    [14, 16, 27, 25],
    [15, 26, 36, 24],
    [19, 20, 32, 31],
    [21, 33, 34, 22],
    [23, 31, 43, 35],
    [25, 37, 38, 26],
    [27, 39, 48, 37],
    [28, 29, 41, 40],
    [30, 42, 44, 32],
    [33, 35, 46, 45],
    [34, 45, 50, 39],
    [36, 38, 49, 47],
    [40, 51, 52, 42],
    [41, 47, 55, 51],
    [43, 53, 54, 46],
    [44, 52, 58, 53],
    [48, 50, 57, 56],
    [49, 56, 59, 55],
    [54, 58, 59, 57]
  ]

triangleIndices :: [[Int]]
triangleIndices =
  [ [0, 2, 3],
    [1, 6, 5],
    [4, 9, 12],
    [7, 16, 14],
    [8, 18, 10],
    [11, 21, 22],
    [13, 15, 24],
    [17, 29, 28],
    [19, 31, 23],
    [20, 30, 32],
    [25, 27, 37],
    [26, 38, 36],
    [33, 45, 34],
    [35, 43, 46],
    [39, 50, 48],
    [40, 41, 51],
    [42, 52, 44],
    [47, 49, 55],
    [53, 58, 54],
    [56, 57, 59]
  ]

pentagonIndices :: [[Int]]
pentagonIndices =
  [ [0, 4, 11, 7, 1],
    [2, 5, 13, 17, 8],
    [3, 10, 20, 19, 9],
    [6, 14, 25, 26, 15],
    [12, 23, 35, 33, 21],
    [16, 22, 34, 39, 27],
    [18, 28, 40, 42, 30],
    [24, 36, 47, 41, 29],
    [31, 32, 44, 53, 43],
    [37, 48, 56, 49, 38],
    [45, 46, 54, 57, 50],
    [51, 55, 59, 58, 52]
  ]

faces :: [PolyFace]
faces =
  []

renderMonochromeRhombicosidodecahedron :: IO ()
renderMonochromeRhombicosidodecahedron = do
  let triangles = makeSimilarFaces triangleIndices white
  let squares = makeSimilarFaces squareIndices white
  let pentagons = makeSimilarFaces pentagonIndices white
  renderShadowedPolyFaces triangles vertices
  renderShadowedPolyFaces squares vertices
  renderShadowedPolyFaces pentagons vertices

renderRhombicosidodecahedronFrame :: IO ()
renderRhombicosidodecahedronFrame = do
  polygonMode $= (Line, Line)
  let triangles = makeSimilarFaces triangleIndices green
  let squares = makeSimilarFaces squareIndices green
  let pentagons = makeSimilarFaces pentagonIndices green
  renderShadowedPolyFaces triangles vertices
  renderShadowedPolyFaces squares vertices
  renderShadowedPolyFaces pentagons vertices
  polygonMode $= (Fill, Fill)

renderRhombicosidodecahedron :: IO ()
renderRhombicosidodecahedron = do
  let triangles = makeSimilarFaces triangleIndices yellow
  let squares = makeSimilarFaces squareIndices blue
  let pentagons = makeSimilarFaces pentagonIndices orange
  renderShadowedPolyFaces triangles vertices
  renderShadowedPolyFaces squares vertices
  renderShadowedPolyFaces pentagons vertices
