module P11_Cuboctahedron where

import Graphics.UI.GLUT
import P3_Cube (renderCubeFrame)
import RenderHelper

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (0.0, 0.0, 1.154701),
    (1.0, 0.0, 0.5773503),
    (0.3333333, 0.942809, 0.5773503),
    (-1.0, 0.0, 0.5773503),
    (-0.3333333, -0.942809, 0.5773503),
    (1.0, 0.0, -0.5773503),
    (0.6666667, -0.942809, 0.0),
    (-0.6666667, 0.942809, 0.0),
    (0.3333333, 0.942809, -0.5773503),
    (-1.0, 0.0, -0.5773503),
    (-0.3333333, -0.942809, -0.5773503),
    (0.0, 0.0, -1.154701)
  ]

squareIndices :: [[Int]]
squareIndices =
  [ [0, 2, 7, 3],
    [0, 4, 6, 1],
    [1, 5, 8, 2],
    [3, 9, 10, 4],
    [5, 6, 10, 11],
    [7, 8, 11, 9]
  ]

triangleIndices :: [[Int]]
triangleIndices =
  [ [0, 1, 2],
    [0, 3, 4],
    [1, 6, 5],
    [2, 8, 7],
    [3, 7, 9],
    [4, 10, 6],
    [5, 11, 8],
    [9, 11, 10]
  ]

faces :: [PolyFace]
faces =
  [ -- Squares
    PolyFace (squareIndices !! 0) yellow,
    PolyFace (squareIndices !! 1) blue,
    PolyFace (squareIndices !! 2) brown,
    PolyFace (squareIndices !! 3) brown,
    PolyFace (squareIndices !! 4) yellow,
    PolyFace (squareIndices !! 5) blue
  ]

renderCuboctahedronCutaway_1 :: IO ()
renderCuboctahedronCutaway_1 = do
  preservingMatrix $ do
    rotate 45 $ Vector3 (1.0 :: GLfloat) 0.0 0.0
    scale 0.82 (0.82 :: GLfloat) 0.82
    renderCubeFrame green
  preservingMatrix $ do
    rotate (-35) $ Vector3 0.0 0.0 (1.0 :: GLfloat)
    renderCuboctahedron

renderMonochromeCuboctahedron :: IO ()
renderMonochromeCuboctahedron = do
  let squares = makeSimilarFaces squareIndices white
  let triangles = makeSimilarFaces triangleIndices white
  renderShadowedPolyFaces squares vertices
  renderShadowedPolyFaces triangles vertices
  renderPolygonBoundary $ renderCuboctahedronFrame black

renderCuboctahedronFrame :: Color3 GLfloat -> IO ()
renderCuboctahedronFrame color = do
  polygonMode $= (Line, Line)
  let squares = makeSimilarFaces squareIndices color
  let triangles = makeSimilarFaces triangleIndices color
  renderShadowedPolyFaces squares vertices
  renderShadowedPolyFaces triangles vertices
  polygonMode $= (Fill, Fill)

renderCuboctahedron :: IO ()
renderCuboctahedron = do
  renderShadowedPolyFaces faces vertices
  let triangles = makeSimilarFaces triangleIndices red
  renderShadowedPolyFaces triangles vertices
  renderPolygonBoundary $ renderCuboctahedronFrame black
