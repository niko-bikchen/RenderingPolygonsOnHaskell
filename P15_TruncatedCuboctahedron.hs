module P15_TruncatedCuboctahedron where

import Graphics.UI.GLUT
import RenderHelper

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (0.0, 0.0, 1.024117),
    (0.4314788, 0.0, 0.928785),
    (-0.02106287, 0.4309644, 0.928785),
    (-0.3410582, -0.2642977, 0.928785),
    (0.4104159, 0.4309644, 0.833453),
    (0.7006238, -0.2642977, 0.6986333),
    (-0.3831839, 0.5976311, 0.7381211),
    (-0.3919084, -0.6380712, 0.6986333),
    (-0.7031792, -0.09763107, 0.7381211),
    (0.6584981, 0.5976311, 0.5079694),
    (0.6497736, -0.6380712, 0.4684816),
    (0.948706, -0.09763107, 0.3731496),
    (-0.4638216, 0.8333333, 0.3731496),
    (-0.7242421, 0.3333333, 0.6427891),
    (-0.7540295, -0.4714045, 0.5079694),
    (-0.1227634, -0.9023689, 0.4684816),
    (0.5778604, 0.8333333, 0.1429979),
    (0.9276431, 0.3333333, 0.2778177),
    (0.8978557, -0.4714045, 0.1429979),
    (0.3087154, -0.9023689, 0.3731496),
    (-0.8048797, 0.5690356, 0.2778177),
    (-0.2157394, 1.0, 0.04766598),
    (-0.8470055, -0.5690356, 0.08715377),
    (-0.2157394, -1.0, 0.04766598),
    (0.8470055, 0.5690356, -0.08715377),
    (0.2157394, 1.0, -0.04766598),
    (0.8048797, -0.5690356, -0.2778177),
    (0.2157394, -1.0, -0.04766598),
    (-0.8978557, 0.4714045, -0.1429979),
    (-0.3087154, 0.9023689, -0.3731496),
    (-0.9276431, -0.3333333, -0.2778177),
    (-0.5778604, -0.8333333, -0.1429979),
    (0.7540295, 0.4714045, -0.5079694),
    (0.1227634, 0.9023689, -0.4684816),
    (0.7242421, -0.3333333, -0.6427891),
    (0.4638216, -0.8333333, -0.3731496),
    (-0.948706, 0.09763107, -0.3731496),
    (-0.6497736, 0.6380712, -0.4684816),
    (-0.6584981, -0.5976311, -0.5079694),
    (0.7031792, 0.09763107, -0.7381211),
    (0.3919084, 0.6380712, -0.6986333),
    (0.3831839, -0.5976311, -0.7381211),
    (-0.7006238, 0.2642977, -0.6986333),
    (-0.4104159, -0.4309644, -0.833453),
    (0.3410582, 0.2642977, -0.928785),
    (0.02106287, -0.4309644, -0.928785),
    (-0.4314788, 0.0, -0.928785),
    (0.0, 0.0, -1.024117)
  ]

squareIndices :: [[Int]]
squareIndices =
  [ [0, 1, 4, 2],
    [3, 8, 14, 7],
    [5, 10, 18, 11],
    [6, 12, 20, 13],
    [9, 17, 24, 16],
    [15, 23, 27, 19],
    [21, 25, 33, 29],
    [22, 30, 38, 31],
    [26, 35, 41, 34],
    [28, 37, 42, 36],
    [32, 39, 44, 40],
    [43, 46, 47, 45]
  ]

hexagonIndices :: [[Int]]
hexagonIndices =
  [ [0, 2, 6, 13, 8, 3],
    [1, 5, 11, 17, 9, 4],
    [7, 14, 22, 31, 23, 15],
    [10, 19, 27, 35, 26, 18],
    [12, 21, 29, 37, 28, 20],
    [16, 24, 32, 40, 33, 25],
    [30, 36, 42, 46, 43, 38],
    [34, 41, 45, 47, 44, 39]
  ]

octagonIndices :: [[Int]]
octagonIndices =
  [ [0, 3, 7, 15, 19, 10, 5, 1],
    [2, 4, 9, 16, 25, 21, 12, 6],
    [8, 13, 20, 28, 36, 30, 22, 14],
    [11, 18, 26, 34, 39, 32, 24, 17],
    [23, 31, 38, 43, 45, 41, 35, 27],
    [29, 33, 40, 44, 47, 46, 42, 37]
  ]

faces :: [PolyFace]
faces =
  [ PolyFace (octagonIndices !! 0) yellow,
    PolyFace (octagonIndices !! 1) green,
    PolyFace (octagonIndices !! 2) red,
    PolyFace (octagonIndices !! 3) red,
    PolyFace (octagonIndices !! 4) green,
    PolyFace (octagonIndices !! 5) yellow
  ]

renderMonochromeTruncatedCuboctahedron :: IO ()
renderMonochromeTruncatedCuboctahedron = do
  let octagons = makeSimilarFaces octagonIndices white
  let hexagons = makeSimilarFaces hexagonIndices white
  let squares = makeSimilarFaces squareIndices white
  renderShadowedPolyFaces octagons vertices
  renderShadowedPolyFaces hexagons vertices
  renderShadowedPolyFaces squares vertices

renderTruncatedCuboctahedronFrame :: IO ()
renderTruncatedCuboctahedronFrame = do
  polygonMode $= (Line, Line)
  let octagons = makeSimilarFaces octagonIndices green
  let hexagons = makeSimilarFaces hexagonIndices green
  let squares = makeSimilarFaces squareIndices green
  renderShadowedPolyFaces octagons vertices
  renderShadowedPolyFaces hexagons vertices
  renderShadowedPolyFaces squares vertices
  polygonMode $= (Fill, Fill)

renderTruncatedCuboctahedron :: IO ()
renderTruncatedCuboctahedron = do
  let squares = makeSimilarFaces squareIndices brown
  let hexagons = makeSimilarFaces hexagonIndices blue
  renderShadowedPolyFaces squares vertices
  renderShadowedPolyFaces hexagons vertices
  renderShadowedPolyFaces faces vertices