module P6_TruncatedTetrahedron where

import Graphics.UI.GLUT
import RenderHelper

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (3.0, 1.0, 1.0), -- A.0
    (1.0, 3.0, 1.0), -- B.1
    (1.0, 1.0, 3.0), -- C.2
    (-3.0, -1.0, 1.0), -- D.3
    (-1.0, -3.0, 1.0), -- E.4
    (-1.0, -1.0, 3.0), -- F.5
    (-3.0, 1.0, -1.0), -- G.6
    (-1.0, 3.0, -1.0), -- H.7
    (-1.0, 1.0, -3.0), -- I.8
    (3.0, -1.0, -1.0), -- J.9
    (1.0, -3.0, -1.0), -- K.10
    (1.0, -1.0, -3.0) -- L.11
  ]

hexagonIndices :: [[Int]]
hexagonIndices =
  [ [0, 2, 5, 4, 10, 9],
    [3, 5, 2, 1, 7, 6],
    [10, 4, 3, 6, 8, 11],
    [7, 1, 0, 9, 11, 8]
  ]

triangleIndices :: [[Int]]
triangleIndices =
  [ [1, 2, 0],
    [4, 5, 3],
    [11, 9, 10],
    [8, 6, 7]
  ]

faces :: [PolyFace]
faces =
  [ -- Hexagons
    PolyFace (hexagonIndices !! 0) yellow,
    PolyFace (hexagonIndices !! 1) blue,
    PolyFace (hexagonIndices !! 2) brown,
    PolyFace (hexagonIndices !! 3) red,
    -- Triangles
    PolyFace (triangleIndices !! 0) brown,
    PolyFace (triangleIndices !! 1) red,
    PolyFace (triangleIndices !! 2) blue,
    PolyFace (triangleIndices !! 3) yellow
  ]

renderMonochromeTruncatedTetrahedron :: IO ()
renderMonochromeTruncatedTetrahedron = do
  let triangles = makeSimilarFaces triangleIndices white
  let hexagons = makeSimilarFaces hexagonIndices white
  renderShadowedPolyFaces triangles vertices
  renderShadowedPolyFaces hexagons vertices
  renderPolygonBoundary $ renderTruncatedTetrahedronFrame black

renderTruncatedTetrahedronFrame :: Color3 GLfloat -> IO ()
renderTruncatedTetrahedronFrame color = do
  polygonMode $= (Line, Line)
  let triangles = makeSimilarFaces triangleIndices color
  let hexagons = makeSimilarFaces hexagonIndices color
  renderShadowedPolyFaces triangles vertices
  renderShadowedPolyFaces hexagons vertices
  polygonMode $= (Fill, Fill)

renderTruncatedTetrahedron :: IO ()
renderTruncatedTetrahedron = do
  renderShadowedPolyFaces faces vertices
  renderPolygonBoundary $ renderTruncatedTetrahedronFrame black
