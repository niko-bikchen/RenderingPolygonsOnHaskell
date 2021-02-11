module P12_Icosidodecahedron where

import Graphics.UI.GLUT
import RenderHelper

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (0.0, 0.0, 1.051462),
    (0.618034, 0.0, 0.8506508),
    (0.2763932, 0.5527864, 0.8506508),
    (-0.618034, 0.0, 0.8506508),
    (-0.2763932, -0.5527864, 0.8506508),
    (1.0, 0.0, 0.3249197),
    (0.7236068, -0.5527864, 0.5257311),
    (-0.1708204, 0.8944272, 0.5257311),
    (0.4472136, 0.8944272, 0.3249197),
    (-1.0, 0.0, 0.3249197),
    (-0.7236068, 0.5527864, 0.5257311),
    (0.1708204, -0.8944272, 0.5257311),
    (-0.4472136, -0.8944272, 0.3249197),
    (1.0, 0.0, -0.3249197),
    (0.8944272, 0.5527864, 0.0),
    (0.5527864, -0.8944272, 0.0),
    (-0.5527864, 0.8944272, 0.0),
    (0.4472136, 0.8944272, -0.3249197),
    (-1.0, 0.0, -0.3249197),
    (-0.8944272, -0.5527864, 0.0),
    (-0.4472136, -0.8944272, -0.3249197),
    (0.618034, 0.0, -0.8506508),
    (0.7236068, -0.5527864, -0.5257311),
    (0.1708204, -0.8944272, -0.5257311),
    (-0.7236068, 0.5527864, -0.5257311),
    (-0.1708204, 0.8944272, -0.5257311),
    (0.2763932, 0.5527864, -0.8506508),
    (-0.618034, 0.0, -0.8506508),
    (-0.2763932, -0.5527864, -0.8506508),
    (0.0, 0.0, -1.051462)
  ]

pentagonIndices :: [[Int]]
pentagonIndices =
  [ [0, 2, 7, 10, 3],
    [0, 4, 11, 6, 1],
    [1, 5, 14, 8, 2],
    [3, 9, 19, 12, 4],
    [5, 6, 15, 22, 13],
    [7, 8, 17, 25, 16],
    [9, 10, 16, 24, 18],
    [11, 12, 20, 23, 15],
    [13, 21, 26, 17, 14],
    [18, 27, 28, 20, 19],
    [21, 22, 23, 28, 29],
    [24, 25, 26, 29, 27]
  ]

triangleIndices :: [[Int]]
triangleIndices =
  [ [0, 1, 2],
    [0, 3, 4],
    [1, 6, 5],
    [2, 8, 7],
    [3, 10, 9],
    [4, 12, 11],
    [5, 13, 14],
    [6, 11, 15],
    [7, 16, 10],
    [8, 14, 17],
    [9, 18, 19],
    [12, 19, 20],
    [13, 22, 21],
    [15, 23, 22],
    [16, 25, 24],
    [17, 26, 25],
    [18, 24, 27],
    [20, 28, 23],
    [21, 29, 26],
    [27, 29, 28]
  ]

faces :: [PolyFace]
faces =
  [ -- First half
    PolyFace (pentagonIndices !! 0) blue,
    PolyFace (pentagonIndices !! 1) orange,
    PolyFace (pentagonIndices !! 2) red,
    PolyFace (pentagonIndices !! 3) yellow,
    PolyFace (pentagonIndices !! 5) yellow,
    PolyFace (pentagonIndices !! 6) red,
    -- Second half
    PolyFace (pentagonIndices !! 8) orange,
    PolyFace (pentagonIndices !! 9) orange,
    PolyFace (pentagonIndices !! 10) yellow,
    PolyFace (pentagonIndices !! 11) blue,
    PolyFace (pentagonIndices !! 7) red,
    PolyFace (pentagonIndices !! 4) blue
  ]

renderMonochromeIcosidodecahedron :: IO ()
renderMonochromeIcosidodecahedron = do
  let pentagons = makeSimilarFaces pentagonIndices white
  let triangles = makeSimilarFaces triangleIndices white
  renderShadowedPolyFaces pentagons vertices
  renderShadowedPolyFaces triangles vertices

renderIcosidodecahedronFrame :: IO ()
renderIcosidodecahedronFrame = do
  polygonMode $= (Line, Line)
  let pentagons = makeSimilarFaces pentagonIndices green
  let triangles = makeSimilarFaces triangleIndices green
  renderShadowedPolyFaces pentagons vertices
  renderShadowedPolyFaces triangles vertices
  polygonMode $= (Fill, Fill)

renderIcosidodecahedron :: IO ()
renderIcosidodecahedron = do
  rotate 140 $ Vector3 0.0 (1.0 :: GLfloat) 0.0
  renderShadowedPolyFaces faces vertices
  let triangles = makeSimilarFaces triangleIndices green
  renderShadowedPolyFaces triangles vertices
