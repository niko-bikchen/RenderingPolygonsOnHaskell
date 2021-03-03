module P17_SnubCube where

import Graphics.UI.GLUT
import RenderHelper

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (0.0, 0.0, 1.077364),
    (0.7442063, 0.0, 0.7790187),
    (0.3123013, 0.6755079, 0.7790187),
    (-0.482096, 0.5669449, 0.7790187),
    (-0.7169181, -0.1996786, 0.7790187),
    (-0.1196038, -0.7345325, 0.7790187),
    (0.6246025, -0.7345325, 0.4806734),
    (1.056508, -0.1996786, 0.06806912),
    (0.8867128, 0.5669449, 0.2302762),
    (0.2621103, 1.042774, 0.06806912),
    (-0.532287, 0.9342111, 0.06806912),
    (-1.006317, 0.3082417, 0.2302762),
    (-0.7020817, -0.784071, 0.2302762),
    (0.02728827, -1.074865, 0.06806912),
    (0.6667271, -0.784071, -0.3184664),
    (0.8216855, -0.09111555, -0.6908285),
    (0.6518908, 0.6755079, -0.5286215),
    (-0.1196038, 0.8751866, -0.6168117),
    (-0.8092336, 0.4758293, -0.5286215),
    (-0.9914803, -0.2761507, -0.3184664),
    (-0.4467414, -0.825648, -0.5286215),
    (0.1926974, -0.5348539, -0.915157),
    (0.1846311, 0.2587032, -1.029416),
    (-0.5049987, -0.1406541, -0.9412258)
  ]

squareIndices :: [[Int]]
squareIndices =
  [ [0, 5, 6, 1],
    [2, 9, 10, 3],
    [4, 11, 19, 12],
    [7, 15, 16, 8],
    [13, 20, 21, 14],
    [17, 22, 23, 18]
  ]

triangleIndices :: [[Int]]
triangleIndices =
  [ [0, 1, 2],
    [0, 2, 3],
    [0, 3, 4],
    [0, 4, 5],
    [1, 6, 7],
    [1, 7, 8],
    [1, 8, 2],
    [2, 8, 9],
    [3, 10, 11],
    [3, 11, 4],
    [4, 12, 5],
    [5, 12, 13],
    [5, 13, 6],
    [6, 13, 14],
    [6, 14, 7],
    [7, 14, 15],
    [8, 16, 9],
    [9, 16, 17],
    [9, 17, 10],
    [10, 17, 18],
    [10, 18, 11],
    [11, 18, 19],
    [12, 19, 20],
    [12, 20, 13],
    [14, 21, 15],
    [15, 21, 22],
    [15, 22, 16],
    [16, 22, 17],
    [18, 23, 19],
    [19, 23, 20],
    [20, 23, 21],
    [21, 23, 22]
  ]

faces :: [PolyFace]
faces =
  [ -- First patch
    PolyFace (squareIndices !! 0) yellow,
    PolyFace (triangleIndices !! 0) blue,
    PolyFace (triangleIndices !! 3) blue,
    PolyFace (triangleIndices !! 4) blue,
    PolyFace (triangleIndices !! 12) blue,
    -- Second patch
    PolyFace (squareIndices !! 3) brown,
    PolyFace (triangleIndices !! 15) yellow,
    PolyFace (triangleIndices !! 16) yellow,
    PolyFace (triangleIndices !! 5) yellow,
    PolyFace (triangleIndices !! 26) yellow,
    -- Third patch
    PolyFace (squareIndices !! 1) blue,
    PolyFace (triangleIndices !! 1) brown,
    PolyFace (triangleIndices !! 7) brown,
    PolyFace (triangleIndices !! 8) brown,
    PolyFace (triangleIndices !! 18) brown,
    -- Fourth patch
    PolyFace (squareIndices !! 4) blue,
    PolyFace (triangleIndices !! 13) brown,
    PolyFace (triangleIndices !! 23) brown,
    PolyFace (triangleIndices !! 24) brown,
    PolyFace (triangleIndices !! 30) brown,
    -- Fifth patch
    PolyFace (squareIndices !! 2) brown,
    PolyFace (triangleIndices !! 10) yellow,
    PolyFace (triangleIndices !! 9) yellow,
    PolyFace (triangleIndices !! 22) yellow,
    PolyFace (triangleIndices !! 21) yellow,
    -- Sixth patch
    PolyFace (squareIndices !! 5) yellow,
    PolyFace (triangleIndices !! 19) blue,
    PolyFace (triangleIndices !! 27) blue,
    PolyFace (triangleIndices !! 28) blue,
    PolyFace (triangleIndices !! 31) blue,
    --
    PolyFace (triangleIndices !! 2) red,
    PolyFace (triangleIndices !! 6) red,
    PolyFace (triangleIndices !! 11) red,
    PolyFace (triangleIndices !! 14) red,
    PolyFace (triangleIndices !! 17) red,
    PolyFace (triangleIndices !! 20) red,
    PolyFace (triangleIndices !! 25) red,
    PolyFace (triangleIndices !! 29) red
  ]

renderMonochromeSnubCube :: IO ()
renderMonochromeSnubCube = do
  let triangles = makeSimilarFaces triangleIndices white
  let squares = makeSimilarFaces squareIndices white
  renderShadowedPolyFaces triangles vertices
  renderShadowedPolyFaces squares vertices
  renderPolygonBoundary $ renderSnubCubeFrame black

renderSnubCubeFrame :: Color3 GLfloat -> IO ()
renderSnubCubeFrame color = do
  polygonMode $= (Line, Line)
  let triangles = makeSimilarFaces triangleIndices color
  let squares = makeSimilarFaces squareIndices color
  renderShadowedPolyFaces triangles vertices
  renderShadowedPolyFaces squares vertices
  polygonMode $= (Fill, Fill)

renderSnubCube :: IO ()
renderSnubCube = do
  rotate 140 $ Vector3 0.0 (1.0 :: GLfloat) 0.0
  renderShadowedPolyFaces faces vertices
  renderPolygonBoundary $ renderSnubCubeFrame black
