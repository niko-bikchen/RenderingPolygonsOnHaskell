module P22_GreatStellatedDodecahedron where

import Graphics.UI.GLUT
import P4_Icosahedron
import RenderHelper

renderGreatStellatedDodecahedronFrameCutaway_1 :: IO ()
renderGreatStellatedDodecahedronFrameCutaway_1 = do
  renderIcosahedron
  polygonMode $= (Line, Line)
  let monochromeFaces = makeSimilarFaces indices green
  renderShadowedSpikes monochromeFaces vertices 3.0
  polygonMode $= (Fill, Fill)

renderMonochromeGreatStellatedDodecahedron :: IO ()
renderMonochromeGreatStellatedDodecahedron = do
  let monochromeFaces = makeSimilarFaces indices white
  renderShadowedSpikes monochromeFaces vertices 3.0
  renderPolygonBoundary $ renderGreatStellatedDodecahedronFrame black

renderGreatStellatedDodecahedronFrame :: Color3 GLfloat -> IO ()
renderGreatStellatedDodecahedronFrame color = do
  polygonMode $= (Line, Line)
  let monochromeFaces = makeSimilarFaces indices color
  renderShadowedSpikes monochromeFaces vertices 3.0
  polygonMode $= (Fill, Fill)

renderGreatStellatedDodecahedron :: IO ()
renderGreatStellatedDodecahedron = do
  renderShadowedSpikes faces vertices 3.0
  renderPolygonBoundary $ renderGreatStellatedDodecahedronFrame black
