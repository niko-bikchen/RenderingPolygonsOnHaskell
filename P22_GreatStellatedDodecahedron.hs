module P22_GreatStellatedDodecahedron where

import Graphics.UI.GLUT
import P4_Icosahedron
import RenderHelper

renderGreatStellatedDodecahedron :: IO ()
renderGreatStellatedDodecahedron = do
  renderShadowedSpikes faces vertices 3.0

renderMonochromeGreatStellatedDodecahedron :: IO ()
renderMonochromeGreatStellatedDodecahedron = do
  let monochromeFaces = makeSimilarFaces indices white
  renderShadowedSpikes monochromeFaces vertices 3.0

renderGreatStellatedDodecahedronFrame :: IO ()
renderGreatStellatedDodecahedronFrame = do
  polygonMode $= (Line, Line)
  let monochromeFaces = makeSimilarFaces indices green
  renderShadowedSpikes monochromeFaces vertices 3.0
  polygonMode $= (Fill, Fill)

renderGreatStellatedDodecahedronFrameCutaway_1 :: IO ()
renderGreatStellatedDodecahedronFrameCutaway_1 = do
  renderIcosahedron
  polygonMode $= (Line, Line)
  let monochromeFaces = makeSimilarFaces indices green
  renderShadowedSpikes monochromeFaces vertices 3.0
  polygonMode $= (Fill, Fill)
