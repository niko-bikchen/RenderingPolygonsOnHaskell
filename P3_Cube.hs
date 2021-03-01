module P3_Cube where

import Graphics.UI.GLUT
import RenderHelper

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (1.0, 1.0, -1.0),
    (-1.0, 1.0, -1.0),
    (-1.0, 1.0, 1.0),
    (1.0, 1.0, 1.0),
    (1.0, -1.0, 1.0),
    (-1.0, -1.0, 1.0),
    (-1.0, -1.0, -1.0),
    (1.0, -1.0, -1.0)
  ]

indices :: [[Int]]
indices =
  [ [0, 1, 2, 3],
    [4, 5, 6, 7],
    [3, 2, 5, 4],
    [7, 6, 1, 0],
    [2, 1, 6, 5],
    [0, 3, 4, 7]
  ]

faces :: [PolyFace]
faces =
  [ PolyFace (indices !! 0) yellow,
    PolyFace (indices !! 1) yellow,
    PolyFace (indices !! 2) blue,
    PolyFace (indices !! 3) blue,
    PolyFace (indices !! 4) brown,
    PolyFace (indices !! 5) brown
  ]


renderMonochromeCube :: IO ()
renderMonochromeCube = do
  let faces = makeSimilarFaces indices white
  renderShadowedPolyFaces faces vertices
  renderPolygonBoundary $ renderCubeFrame black

renderCubeFrame :: Color3 GLfloat -> IO ()
renderCubeFrame color = do
  polygonMode $= (Line, Line)
  let faces = makeSimilarFaces indices color
  renderShadowedPolyFaces faces vertices
  polygonMode $= (Fill, Fill)

renderCube :: IO ()
renderCube = do
  renderShadowedPolyFaces faces vertices
  renderPolygonBoundary $ renderCubeFrame black
