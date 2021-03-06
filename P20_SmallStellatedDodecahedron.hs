module P20_SmallStellatedDodecahedron where

import Graphics.UI.GLUT
import P5_Dodecahedron
  ( faces,
    indices,
    renderDodecahedron,
    vertices,
  )
import RenderHelper

renderSmallStellatedDodecahedronCutaway_1 :: IO ()
renderSmallStellatedDodecahedronCutaway_1 = do
  renderDodecahedron
  polygonMode $= (Line, Line)
  let monochromeFaces = makeSimilarFaces indices green
  renderShadowedSpikes monochromeFaces vertices 2.0
  polygonMode $= (Fill, Fill)

renderSmallStellatedDodecahedronFrame :: Color3 GLfloat -> IO ()
renderSmallStellatedDodecahedronFrame color = do
  polygonMode $= (Line, Line)
  let monochromeFaces = makeSimilarFaces indices color
  renderShadowedSpikes monochromeFaces vertices 2.0
  polygonMode $= (Fill, Fill)

renderSmallStellatedDodecahedron :: IO ()
renderSmallStellatedDodecahedron = do
  renderShadowedSpikes faces vertices 2.0
  renderPolygonBoundary $ renderSmallStellatedDodecahedronFrame black

renderMonochromeSmallStellatedDodecahedron :: IO ()
renderMonochromeSmallStellatedDodecahedron = do
  let monochromeFaces = makeSimilarFaces indices white
  renderShadowedSpikes monochromeFaces vertices 2.0
  renderPolygonBoundary $ renderSmallStellatedDodecahedronFrame black
