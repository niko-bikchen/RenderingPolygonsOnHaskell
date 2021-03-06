module P7_TruncatedOctahedron where

import Graphics.UI.GLUT
import RenderHelper

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (0.0, 0.0, 1.054093),
    (0.6324555, 0.0, 0.843274),
    (-0.421637, 0.4714045, 0.843274),
    (-0.07027284, -0.6285394, 0.843274),
    (0.843274, 0.4714045, 0.421637),
    (0.5621827, -0.6285394, 0.6324555),
    (-0.9135469, 0.3142697, 0.421637),
    (-0.2108185, 0.942809, 0.421637),
    (-0.5621827, -0.7856742, 0.421637),
    (0.9838197, 0.3142697, -0.2108185),
    (0.421637, 0.942809, 0.2108185),
    (0.7027284, -0.7856742, 0),
    (-0.7027284, 0.7856742, 0),
    (-0.9838197, -0.3142697, 0.2108185),
    (-0.421637, -0.942809, -0.2108185),
    (0.5621827, 0.7856742, -0.421637),
    (0.9135469, -0.3142697, -0.421637),
    (0.2108185, -0.942809, -0.421637),
    (-0.5621827, 0.6285394, -0.6324555),
    (-0.843274, -0.4714045, -0.421637),
    (0.07027284, 0.6285394, -0.843274),
    (0.421637, -0.4714045, -0.843274),
    (-0.6324555, 0, -0.843274),
    (0.0, 0.0, -1.054093)
  ]

hexagonIndices :: [[Int]]
hexagonIndices =
  [ [0, 1, 4, 10, 7, 2],
    [0, 2, 6, 13, 8, 3],
    [1, 5, 11, 16, 9, 4],
    [3, 8, 14, 17, 11, 5],
    [6, 12, 18, 22, 19, 13],
    [7, 10, 15, 20, 18, 12],
    [9, 16, 21, 23, 20, 15],
    [14, 19, 22, 23, 21, 17]
  ]

squareIndices :: [[Int]]
squareIndices =
  [ [0, 3, 5, 1],
    [2, 7, 12, 6],
    [4, 9, 15, 10],
    [8, 13, 19, 14],
    [11, 17, 21, 16],
    [18, 20, 23, 22]
  ]

faces :: [PolyFace]
faces =
  [ -- Hexagons
    PolyFace (hexagonIndices !! 0) yellow,
    PolyFace (hexagonIndices !! 1) blue,
    PolyFace (hexagonIndices !! 2) brown,
    PolyFace (hexagonIndices !! 5) red,
    PolyFace (hexagonIndices !! 7) yellow,
    PolyFace (hexagonIndices !! 4) brown,
    PolyFace (hexagonIndices !! 3) red,
    PolyFace (hexagonIndices !! 6) blue
  ]

renderTruncatedOctahedronCutaway_1 :: IO ()
renderTruncatedOctahedronCutaway_1 = do
  renderTruncatedOctahedron
  polygonMode $= (Line, Line)
  let monochromeFaces = makeSimilarFaces squareIndices blue
  renderShadowedSpikes monochromeFaces vertices 1.4
  polygonMode $= (Fill, Fill)

renderMonochromeTruncatedOctahedron :: IO ()
renderMonochromeTruncatedOctahedron = do
  let squares = makeSimilarFaces squareIndices white
  let hexagons = makeSimilarFaces hexagonIndices white
  renderShadowedPolyFaces squares vertices
  renderShadowedPolyFaces hexagons vertices
  renderPolygonBoundary $ renderTruncatedOctahedronFrame black

renderTruncatedOctahedronFrame :: Color3 GLfloat -> IO ()
renderTruncatedOctahedronFrame color = do
  polygonMode $= (Line, Line)
  let squares = makeSimilarFaces squareIndices color
  let hexagons = makeSimilarFaces hexagonIndices color
  renderShadowedPolyFaces squares vertices
  renderShadowedPolyFaces hexagons vertices
  polygonMode $= (Fill, Fill)

renderTruncatedOctahedron :: IO ()
renderTruncatedOctahedron = do
  renderShadowedPolyFaces faces vertices
  let squares = makeSimilarFaces squareIndices green
  renderShadowedPolyFaces squares vertices
  renderPolygonBoundary $ renderTruncatedOctahedronFrame black
