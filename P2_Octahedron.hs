module P2_Octahedron where

import Graphics.UI.GLUT
import RenderHelper

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (1.0, 0.0, 0.0),
    (-1.0, 0.0, 0.0),
    (0.0, 1.0, 0.0),
    (0.0, -1.0, 0.0),
    (0.0, 0.0, 1.0),
    (0.0, 0.0, -1.0)
  ]

indices :: [[Int]]
indices =
  [ [4, 3, 0],
    [4, 0, 2],
    [4, 2, 1],
    [4, 1, 3],
    [0, 3, 5],
    [3, 1, 5],
    [1, 2, 5],
    [2, 0, 5]
  ]

faces :: [PolyFace]
faces =
  [ PolyFace (indices !! 0) yellow,
    PolyFace (indices !! 1) blue,
    PolyFace (indices !! 2) brown,
    PolyFace (indices !! 3) red,
    PolyFace (indices !! 4) red,
    PolyFace (indices !! 5) yellow,
    PolyFace (indices !! 6) blue,
    PolyFace (indices !! 7) brown
  ]

renderMonochromeOctahedron :: IO ()
renderMonochromeOctahedron = do
  let faces = makeSimilarFaces indices white
  renderShadowedPolyFaces faces vertices

renderOctahedronFrame :: IO ()
renderOctahedronFrame = do
  polygonMode $= (Line, Line)
  let faces = makeSimilarFaces indices green
  renderShadowedPolyFaces faces vertices
  polygonMode $= (Fill, Fill)

renderOctahedron :: IO ()
renderOctahedron = renderShadowedPolyFaces faces vertices