module P6_TruncatedTetrahedron where

import Graphics.UI.GLUT
import RenderHelper

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (0.0, 0.0, 1.105542),
    (0.8528029, 0.0, 0.7035265),
    (-0.7106691, 0.4714045, 0.7035265),
    (0.3316456, -0.7856742, 0.7035265),
    (0.9949367, 0.4714045, -0.1005038),
    (-1.089693, 0.1571348, -0.1005038),
    (-0.5685352, 0.942809, -0.1005038),
    (-0.04737794, -1.099944, -0.1005038),
    (0.6159132, 0.1571348, -0.904534),
    (0.2842676, 0.942809, -0.5025189),
    (-0.758047, -0.6285394, -0.5025189),
    (0.09475587, -0.6285394, -0.904534)
  ]

hexagonIndices :: [[Int]]
hexagonIndices =
  [ [0, 1, 4, 9, 6, 2],
    [0, 2, 5, 10, 7, 3],
    [1, 3, 7, 11, 8, 4],
    [5, 6, 9, 8, 11, 10]
  ]

triangleIndices :: [[Int]]
triangleIndices =
  [ [0, 3, 1],
    [2, 6, 5],
    [4, 8, 9],
    [7, 10, 11]
  ]

faces :: [PolyFace]
faces =
  [ -- Hexagons
    PolyFace (hexagonIndices !! 0) yellow,
    PolyFace (hexagonIndices !! 1) blue,
    PolyFace (hexagonIndices !! 2) brown,
    PolyFace (hexagonIndices !! 3) red,
    -- Triangles
    PolyFace (triangleIndices !! 0) red,
    PolyFace (triangleIndices !! 1) brown,
    PolyFace (triangleIndices !! 2) blue,
    PolyFace (triangleIndices !! 3) yellow
  ]

renderTruncatedTetrahedronCutaway_1 :: IO ()
renderTruncatedTetrahedronCutaway_1 = do
  renderTruncatedTetrahedron
  polygonMode $= (Line, Line)
  let monochromeFaces = makeSimilarFaces triangleIndices green
  renderShadowedSpikes monochromeFaces vertices 1.7
  polygonMode $= (Fill, Fill)

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
