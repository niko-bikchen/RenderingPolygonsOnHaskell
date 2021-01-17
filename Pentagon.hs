module Pentagon where

import Graphics.UI.GLUT
import RenderHelper

renderPentagon :: [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderPentagon = renderVerticesAs Polygon

renderPentagonsByIndices :: [(Int, Int, Int, Int, Int)] -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderPentagonsByIndices = renderVerticesBy5IndicesAs Polygon

renderPentagonByIndices :: (Int, Int, Int, Int, Int) -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderPentagonByIndices indices = renderVerticesBy5IndicesAs Polygon [indices]