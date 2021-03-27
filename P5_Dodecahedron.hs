module P5_Dodecahedron where

import Graphics.UI.GLUT
import P3_Cube (renderCube)
import RenderHelper

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (0, 0, 1.070466),
    (0.7136442, 0, 0.7978784),
    (-0.3568221, 0.618034, 0.7978784),
    (-0.3568221, -0.618034, 0.7978784),
    (0.7978784, 0.618034, 0.3568221),
    (0.7978784, -0.618034, 0.3568221),
    (-0.9341724, 0.381966, 0.3568221),
    (0.1362939, 1, 0.3568221),
    (0.1362939, -1, 0.3568221),
    (-0.9341724, -0.381966, 0.3568221),
    (0.9341724, 0.381966, -0.3568221),
    (0.9341724, -0.381966, -0.3568221),
    (-0.7978784, 0.618034, -0.3568221),
    (-0.1362939, 1, -0.3568221),
    (-0.1362939, -1, -0.3568221),
    (-0.7978784, -0.618034, -0.3568221),
    (0.3568221, 0.618034, -0.7978784),
    (0.3568221, -0.618034, -0.7978784),
    (-0.7136442, 0, -0.7978784),
    (0, 0, -1.070466)
  ]

indices :: [[Int]]
indices =
  [ [0, 1, 4, 7, 2],
    [0, 2, 6, 9, 3],
    [0, 3, 8, 5, 1],
    [1, 5, 11, 10, 4],
    [2, 7, 13, 12, 6],
    [3, 9, 15, 14, 8],
    [4, 10, 16, 13, 7],
    [5, 8, 14, 17, 11],
    [6, 12, 18, 15, 9],
    [10, 11, 17, 19, 16],
    [12, 13, 16, 19, 18],
    [14, 15, 18, 19, 17]
  ]

faces :: [PolyFace]
faces =
  [ -- Top "hat"
    PolyFace (indices !! 2) red,
    PolyFace (indices !! 0) brown,
    PolyFace (indices !! 1) yellow,
    PolyFace (indices !! 3) blue,
    PolyFace (indices !! 5) blue,
    PolyFace (indices !! 7) brown,
    -- Bottom "hat"
    PolyFace (indices !! 10) brown,
    PolyFace (indices !! 6) yellow,
    PolyFace (indices !! 4) blue,
    PolyFace (indices !! 8) red,
    PolyFace (indices !! 11) yellow,
    PolyFace (indices !! 9) red
  ]

renderDodecahedronCutaway_1 :: IO ()
renderDodecahedronCutaway_1 = do
  renderCube
  preservingMatrix $ do
    rotate (-20) $ Vector3 0.0 (1.0 :: GLfloat) 0.0
    scale 1.62 (1.62 :: GLfloat) 1.62
    lineWidth $= 2
    renderDodecahedronFrame green
    lineWidth $= 1

renderMonochromeDodecahedron :: IO ()
renderMonochromeDodecahedron = do
  let faces = makeSimilarFaces indices white
  renderShadowedPolyFaces faces vertices
  renderPolygonBoundary $ renderDodecahedronFrame black

renderDodecahedronFrame :: Color3 GLfloat -> IO ()
renderDodecahedronFrame color = do
  polygonMode $= (Line, Line)
  let faces = makeSimilarFaces indices color
  renderShadowedPolyFaces faces vertices
  polygonMode $= (Fill, Fill)

renderDodecahedron :: IO ()
renderDodecahedron = do
  renderShadowedPolyFaces faces vertices
  renderPolygonBoundary $ renderDodecahedronFrame black
