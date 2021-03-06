module P23_CubeAndOctahedron where

import Graphics.UI.GLUT
import P2_Octahedron
import P3_Cube
import RenderHelper

cubeFaces :: [PolyFace]
cubeFaces =
  [ PolyFace (P3_Cube.indices !! 0) orange,
    PolyFace (P3_Cube.indices !! 1) orange,
    PolyFace (P3_Cube.indices !! 2) red,
    PolyFace (P3_Cube.indices !! 3) red,
    PolyFace (P3_Cube.indices !! 4) green,
    PolyFace (P3_Cube.indices !! 5) green
  ]

octahedronFaces :: [PolyFace]
octahedronFaces =
  [ PolyFace (P2_Octahedron.indices !! 0) yellow,
    PolyFace (P2_Octahedron.indices !! 1) blue,
    PolyFace (P2_Octahedron.indices !! 2) yellow,
    PolyFace (P2_Octahedron.indices !! 3) blue,
    PolyFace (P2_Octahedron.indices !! 4) blue,
    PolyFace (P2_Octahedron.indices !! 5) yellow,
    PolyFace (P2_Octahedron.indices !! 6) blue,
    PolyFace (P2_Octahedron.indices !! 7) yellow
  ]

renderCubeAndOctahedronCutaway_1 :: IO ()
renderCubeAndOctahedronCutaway_1 = do
  renderShadowedPolyFaces cubeFaces P3_Cube.vertices
  renderPolygonBoundary $ renderCubeFrame black
  preservingMatrix $ do
    rotate 90 $ Vector3 (1.0 :: GLfloat) 0.0 0.0
    scale 2.0 (2.0 :: GLfloat) 2.0
    renderOctahedronFrame blue

renderCubeAndOctahedronFrame :: IO ()
renderCubeAndOctahedronFrame = do
  renderCubeFrame green
  preservingMatrix $ do
    rotate 90 $ Vector3 (1.0 :: GLfloat) 0.0 0.0
    scale 2.0 (2.0 :: GLfloat) 2.0
    renderOctahedronFrame green

renderCubeAndOctahedronMonochrome :: IO ()
renderCubeAndOctahedronMonochrome = do
  renderMonochromeCube
  preservingMatrix $ do
    rotate 90 $ Vector3 (1.0 :: GLfloat) 0.0 0.0
    scale 2.0 (2.0 :: GLfloat) 2.0
    renderMonochromeOctahedron

renderCubeAndOctahedron :: IO ()
renderCubeAndOctahedron = do
  renderShadowedPolyFaces cubeFaces P3_Cube.vertices
  renderPolygonBoundary $ renderCubeFrame black
  preservingMatrix $ do
    rotate 90 $ Vector3 (1.0 :: GLfloat) 0.0 0.0
    scale 2.0 (2.0 :: GLfloat) 2.0
    renderShadowedPolyFaces octahedronFaces P2_Octahedron.vertices
    renderPolygonBoundary $ renderOctahedronFrame black