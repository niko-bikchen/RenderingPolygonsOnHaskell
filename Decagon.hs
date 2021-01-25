module Decagon where

import Graphics.UI.GLUT
import RenderHelper

renderDecagon :: [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderDecagon = renderVerticesAs Polygon

renderDecagonsByIndices :: [(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)] -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderDecagonsByIndices = renderVerticesBy10IndicesAs Polygon

renderDecagonByIndices :: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderDecagonByIndices indices = renderVerticesBy10IndicesAs Polygon [indices]