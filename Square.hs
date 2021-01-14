module Square where

import Graphics.UI.GLUT
import RenderHelper

renderSquare :: [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderSquare = renderVerticesAs Quads

renderSquaresByIndices :: [(Int, Int, Int, Int)] -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderSquaresByIndices = renderVerticesBy4IndicesAs Quads

renderSquareByIndices :: (Int, Int, Int, Int) -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderSquareByIndices indices = renderVerticesBy4IndicesAs Quads [indices]
