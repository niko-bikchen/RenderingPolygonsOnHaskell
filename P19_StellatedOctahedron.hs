module P19_StellatedOctahedron where

import Graphics.UI.GLUT
import P1_Tetrahedron (renderCustomMonochromeTetrahedron, renderTetrahedron, renderTetrahedronFrame)
import P2_Octahedron (indices, renderOctahedron, vertices)
import RenderHelper

constant :: GLfloat
constant = sqrt 2 / 4

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (constant, constant, constant),
    (constant, constant, - constant),
    (constant, - constant, constant),
    (constant, - constant, - constant),
    (- constant, constant, constant),
    (- constant, constant, - constant),
    (- constant, - constant, constant),
    (- constant, - constant, - constant)
  ]

triangleIndices :: [[Int]]
triangleIndices =
  [ [2, 1, 4],
    [2, 4, 7],
    [2, 7, 1],
    [1, 7, 4],
    [0, 3, 5],
    [0, 5, 6],
    [0, 6, 3],
    [3, 6, 5]
  ]

faces :: [PolyFace]
faces =
  [ PolyFace (triangleIndices !! 0) brown,
    PolyFace (triangleIndices !! 1) blue,
    PolyFace (triangleIndices !! 2) yellow,
    PolyFace (triangleIndices !! 3) red,
    PolyFace (triangleIndices !! 4) blue,
    PolyFace (triangleIndices !! 5) yellow,
    PolyFace (triangleIndices !! 6) red,
    PolyFace (triangleIndices !! 7) brown
  ]

renderStellatedOctahedronCutaway_3 :: IO ()
renderStellatedOctahedronCutaway_3 = do
  renderOctahedron
  polygonMode $= (Line, Line)
  let monochromeFaces = makeSimilarFaces indices green
  renderShadowedSpikes monochromeFaces P2_Octahedron.vertices 1.7
  polygonMode $= (Fill, Fill)

renderStellatedOctahedronCutaway_2 :: IO ()
renderStellatedOctahedronCutaway_2 = do
  renderOctahedron
  scale 0.5 0.5 (0.5 :: GLfloat)
  preservingMatrix $ do
    translate $ Vector3 1.3 (-1.3 :: GLfloat) (-1.3)
    renderCustomMonochromeTetrahedron green
  preservingMatrix $ do
    translate $ Vector3 (-1.3) (1.3 :: GLfloat) (-1.3)
    renderCustomMonochromeTetrahedron green
  preservingMatrix $ do
    translate $ Vector3 1.3 (-1.3 :: GLfloat) 1.3
    rotate 90 $ Vector3 0.0 (1.0 :: GLfloat) 0.0
    renderCustomMonochromeTetrahedron green
  preservingMatrix $ do
    translate $ Vector3 (-1.3) (1.3 :: GLfloat) 1.3
    rotate 90 $ Vector3 0.0 (1.0 :: GLfloat) 0.0
    renderCustomMonochromeTetrahedron green
  preservingMatrix $ do
    translate $ Vector3 (-1.3) (-1.3 :: GLfloat) 1.3
    rotate 180 $ Vector3 0.0 (1.0 :: GLfloat) 0.0
    renderCustomMonochromeTetrahedron green
  preservingMatrix $ do
    translate $ Vector3 1.3 (1.3 :: GLfloat) 1.3
    rotate 180 $ Vector3 0.0 (1.0 :: GLfloat) 0.0
    renderCustomMonochromeTetrahedron green
  preservingMatrix $ do
    translate $ Vector3 (-1.3) (-1.3 :: GLfloat) (-1.3)
    rotate 90 $ Vector3 0.0 (1.0 :: GLfloat) 0.0
    renderCustomMonochromeTetrahedron green
  preservingMatrix $ do
    translate $ Vector3 1.3 (1.3 :: GLfloat) (-1.3)
    rotate 90 $ Vector3 0.0 (1.0 :: GLfloat) 0.0
    renderCustomMonochromeTetrahedron green

renderStellatedOctahedronCutaway_1 :: IO ()
renderStellatedOctahedronCutaway_1 = do
  renderTetrahedron
  rotate 90 $ Vector3 0.0 (1.0 :: GLfloat) 0.0
  renderTetrahedronFrame green

renderMonochromeStellatedOctahedron :: IO ()
renderMonochromeStellatedOctahedron = do
  let monochromeFaces = makeSimilarFaces triangleIndices white
  renderShadowedPolyFaces monochromeFaces P19_StellatedOctahedron.vertices

renderStellatedOctahedronFrame :: IO ()
renderStellatedOctahedronFrame = do
  polygonMode $= (Line, Line)
  let monochromeFaces = makeSimilarFaces triangleIndices green
  renderShadowedPolyFaces monochromeFaces P19_StellatedOctahedron.vertices
  polygonMode $= (Fill, Fill)

renderStellatedOctahedron :: IO ()
renderStellatedOctahedron = do
  rotate 140 $ Vector3 0.0 (1.0 :: GLfloat) 0.0
  renderShadowedPolyFaces faces P19_StellatedOctahedron.vertices
