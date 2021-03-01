module P1_Tetrahedron where

import Graphics.UI.GLUT
import RenderHelper

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (1.0, 1.0, 1.0), -- Vertex A
    (-1.0, 1.0, -1.0), -- Vertex B
    (1.0, -1.0, -1.0), -- Vertex C
    (-1.0, -1.0, 1.0) -- Vertex D
  ]

indices :: [[Int]]
indices =
  [ [3, 1, 2],
    [0, 3, 2],
    [0, 1, 3],
    [0, 2, 1]
  ]

faces :: [PolyFace]
faces =
  [ PolyFace (indices !! 0) yellow,
    PolyFace (indices !! 1) blue,
    PolyFace (indices !! 2) orange,
    PolyFace (indices !! 3) red
  ]

renderMonochromeTetrahedron :: IO ()
renderMonochromeTetrahedron = do
  let faces = makeSimilarFaces indices white
  renderShadowedPolyFaces faces vertices
  renderPolygonBoundary $ renderTetrahedronFrame black

renderCustomMonochromeTetrahedron :: Color3 GLfloat -> IO ()
renderCustomMonochromeTetrahedron color = do
  let faces = makeSimilarFaces indices color
  renderShadowedPolyFaces faces vertices
  renderPolygonBoundary $ renderTetrahedronFrame black

renderTetrahedronFrame :: Color3 GLfloat -> IO ()
renderTetrahedronFrame color = do
  polygonMode $= (Line, Line)
  let faces = makeSimilarFaces indices color
  renderShadowedPolyFaces faces vertices
  polygonMode $= (Fill, Fill)

renderTetrahedron :: IO ()
renderTetrahedron = do
  renderShadowedPolyFaces faces vertices
  renderPolygonBoundary $ renderTetrahedronFrame black
