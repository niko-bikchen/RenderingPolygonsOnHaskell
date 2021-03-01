module P4_Icosahedron where

import Graphics.UI.GLUT
import RenderHelper

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (0, 0, 1.175571),
    (1.051462, 0, 0.5257311),
    (0.3249197, 1, 0.5257311),
    (-0.8506508, 0.618034, 0.5257311),
    (-0.8506508, -0.618034, 0.5257311),
    (0.3249197, -1, 0.5257311),
    (0.8506508, 0.618034, -0.5257311),
    (0.8506508, -0.618034, -0.5257311),
    (-0.3249197, 1, -0.5257311),
    (-1.051462, 0, -0.5257311),
    (-0.3249197, -1, -0.5257311),
    (0, 0, -1.175571)
  ]

indices :: [[Int]]
indices =
  [ [0, 1, 2],
    [0, 2, 3],
    [0, 3, 4],
    [0, 4, 5],
    [0, 5, 1],
    [1, 5, 7],
    [1, 7, 6],
    [1, 6, 2],
    [2, 6, 8],
    [2, 8, 3],
    [3, 8, 9],
    [3, 9, 4],
    [4, 9, 10],
    [4, 10, 5],
    [5, 10, 7],
    [6, 7, 11],
    [6, 11, 8],
    [7, 10, 11],
    [8, 11, 9],
    [9, 11, 10]
  ]

faces :: [PolyFace]
faces =
  [ -- Top "hat"
    PolyFace (indices !! 0) brown,
    PolyFace (indices !! 1) blue,
    PolyFace (indices !! 2) yellow,
    PolyFace (indices !! 3) green,
    PolyFace (indices !! 4) red,
    -- Triangles which come from top
    PolyFace (indices !! 5) blue,
    PolyFace (indices !! 7) yellow,
    PolyFace (indices !! 9) green,
    PolyFace (indices !! 11) red,
    PolyFace (indices !! 13) brown,
    -- Bottom "hat"
    PolyFace (indices !! 19) red,
    PolyFace (indices !! 16) yellow,
    PolyFace (indices !! 18) green,
    PolyFace (indices !! 15) blue,
    PolyFace (indices !! 17) brown,
    -- Triangles which come from bottom
    PolyFace (indices !! 8) brown,
    PolyFace (indices !! 6) red,
    PolyFace (indices !! 10) blue,
    PolyFace (indices !! 12) yellow,
    PolyFace (indices !! 14) green
  ]

renderMonochromeIcosahedron :: IO ()
renderMonochromeIcosahedron = do
  let faces = makeSimilarFaces indices white
  renderShadowedPolyFaces faces vertices
  renderPolygonBoundary $ renderIcosahedronFrame black

renderIcosahedronFrame :: Color3 GLfloat -> IO ()
renderIcosahedronFrame color = do
  polygonMode $= (Line, Line)
  let faces = makeSimilarFaces indices color
  renderShadowedPolyFaces faces vertices
  polygonMode $= (Fill, Fill)

renderIcosahedron :: IO ()
renderIcosahedron = do
  renderShadowedPolyFaces faces vertices
  renderPolygonBoundary $ renderIcosahedronFrame black
