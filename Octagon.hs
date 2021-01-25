module Octagon where

import Graphics.UI.GLUT
import RenderHelper

renderOctagon :: [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderOctagon = renderVerticesAs Polygon

renderOctagonsByIndices :: [(Int, Int, Int, Int, Int, Int, Int, Int)] -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderOctagonsByIndices = renderVerticesBy8IndicesAs Polygon

renderOctagonByIndices :: (Int, Int, Int, Int, Int, Int, Int, Int) -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderOctagonByIndices indices = renderVerticesBy8IndicesAs Polygon [indices]