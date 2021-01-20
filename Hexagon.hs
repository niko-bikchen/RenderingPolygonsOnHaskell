module Hexagon where

import Graphics.UI.GLUT
import RenderHelper

renderHexagon :: [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderHexagon = renderVerticesAs Polygon

renderHexagonsByIndices :: [(Int, Int, Int, Int, Int, Int)] -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderHexagonsByIndices = renderVerticesBy6IndicesAs Polygon

renderHexagonByIndices :: (Int, Int, Int, Int, Int, Int) -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderHexagonByIndices indices = renderVerticesBy6IndicesAs Polygon [indices]