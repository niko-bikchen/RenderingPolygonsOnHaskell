module P22_GreatStellatedDodecahedron where

import Graphics.UI.GLUT
import P4_Icosahedron
import RenderHelper

renderGreatStellatedDodecahedronFrameCutaway_1 :: IO ()
renderGreatStellatedDodecahedronFrameCutaway_1 = do
  scale 0.7 (0.7 :: GLfloat) 0.7
  renderIcosahedron
  polygonMode $= (Line, Line)
  let monochromeFaces = makeSimilarFaces indices green
  renderShadowedSpikes monochromeFaces vertices 3.0
  polygonMode $= (Fill, Fill)

renderMonochromeGreatStellatedDodecahedron :: IO ()
renderMonochromeGreatStellatedDodecahedron = do
  preservingMatrix $ do
    scale 0.7 (0.7 :: GLfloat) 0.7
    let monochromeFaces = makeSimilarFaces indices white
    renderShadowedSpikes monochromeFaces vertices 3.0
  renderPolygonBoundary $ renderGreatStellatedDodecahedronFrame black

renderGreatStellatedDodecahedronFrame :: Color3 GLfloat -> IO ()
renderGreatStellatedDodecahedronFrame color = do
  scale 0.7 (0.7 :: GLfloat) 0.7
  polygonMode $= (Line, Line)
  let monochromeFaces = makeSimilarFaces indices color
  renderShadowedSpikes monochromeFaces vertices 3.0
  polygonMode $= (Fill, Fill)

renderGreatStellatedDodecahedron :: IO ()
renderGreatStellatedDodecahedron = do
  preservingMatrix $ do
    scale 0.7 (0.7 :: GLfloat) 0.7
    renderShadowedSpikes faces vertices 3.0
  renderPolygonBoundary $ renderGreatStellatedDodecahedronFrame black
