module P13_Rhombicuboctahedron where

import Graphics.UI.GLUT
import RenderHelper

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (0.0, 0.0, 1.070722),
    (0.7148135, 0.0, 0.7971752),
    (-0.104682, 0.7071068, 0.7971752),
    (-0.6841528, 0.2071068, 0.7971752),
    (-0.104682, -0.7071068, 0.7971752),
    (0.6101315, 0.7071068, 0.5236279),
    (1.04156, 0.2071068, 0.1367736),
    (0.6101315, -0.7071068, 0.5236279),
    (-0.3574067, 1.0, 0.1367736),
    (-0.7888348, -0.5, 0.5236279),
    (-0.9368776, 0.5, 0.1367736),
    (-0.3574067, -1.0, 0.1367736),
    (0.3574067, 1.0, -0.1367736),
    (0.9368776, -0.5, -0.1367736),
    (0.7888348, 0.5, -0.5236279),
    (0.3574067, -1.0, -0.1367736),
    (-0.6101315, 0.7071068, -0.5236279),
    (-1.04156, -0.2071068, -0.1367736),
    (-0.6101315, -0.7071068, -0.5236279),
    (0.104682, 0.7071068, -0.7971752),
    (0.6841528, -0.2071068, -0.7971752),
    (0.104682, -0.7071068, -0.7971752),
    (-0.7148135, 0.0, -0.7971752),
    (0.0, 0.0, -1.070722)
  ]

squareIndices :: [[Int]]
squareIndices =
  [ [0, 1, 5, 2],
    [0, 3, 9, 4],
    [0, 4, 7, 1],
    [1, 7, 13, 6],
    [2, 5, 12, 8],
    [2, 8, 10, 3],
    [3, 10, 17, 9],
    [4, 11, 15, 7],
    [5, 6, 14, 12],
    [6, 13, 20, 14],
    [8, 12, 19, 16],
    [9, 17, 18, 11],
    [10, 16, 22, 17],
    [11, 18, 21, 15],
    [13, 15, 21, 20],
    [14, 20, 23, 19],
    [16, 19, 23, 22],
    [18, 22, 23, 21]
  ]

triangleIndices :: [[Int]]
triangleIndices =
  [ [0, 2, 3],
    [1, 6, 5],
    [4, 9, 11],
    [7, 15, 13],
    [8, 16, 10],
    [12, 14, 19],
    [17, 22, 18],
    [20, 21, 23]
  ]

faces :: [PolyFace]
faces =
  [ -- First half
    PolyFace (squareIndices !! 0) red,
    PolyFace (squareIndices !! 1) red,
    PolyFace (squareIndices !! 2) yellow,
    PolyFace (squareIndices !! 3) red,
    PolyFace (squareIndices !! 7) red,
    -- Belt
    PolyFace (squareIndices !! 4) yellow,
    PolyFace (squareIndices !! 5) red,
    PolyFace (squareIndices !! 6) yellow,
    PolyFace (squareIndices !! 8) red,
    PolyFace (squareIndices !! 9) yellow,
    PolyFace (squareIndices !! 11) red,
    PolyFace (squareIndices !! 13) yellow,
    PolyFace (squareIndices !! 14) red,
    -- Second half
    PolyFace (squareIndices !! 12) red,
    PolyFace (squareIndices !! 10) red,
    PolyFace (squareIndices !! 15) red,
    PolyFace (squareIndices !! 16) yellow,
    PolyFace (squareIndices !! 17) red
  ]

renderMonochromeRhombicuboctahedron :: IO ()
renderMonochromeRhombicuboctahedron = do
  let squares = makeSimilarFaces squareIndices white
  let triangles = makeSimilarFaces triangleIndices white
  renderShadowedPolyFaces squares vertices
  renderShadowedPolyFaces triangles vertices
  renderPolygonBoundary $ renderRhombicuboctahedronFrame black

renderRhombicuboctahedronFrame :: Color3 GLfloat -> IO ()
renderRhombicuboctahedronFrame color = do
  polygonMode $= (Line, Line)
  let squares = makeSimilarFaces squareIndices color
  let triangles = makeSimilarFaces triangleIndices color
  renderShadowedPolyFaces squares vertices
  renderShadowedPolyFaces triangles vertices
  polygonMode $= (Fill, Fill)

renderRhombicuboctahedron :: IO ()
renderRhombicuboctahedron = do
  renderShadowedPolyFaces faces vertices
  let triangles = makeSimilarFaces triangleIndices blue
  renderShadowedPolyFaces triangles vertices
  renderPolygonBoundary $ renderRhombicuboctahedronFrame black
