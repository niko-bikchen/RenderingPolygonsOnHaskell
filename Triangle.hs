module Triangle where

import Graphics.UI.GLUT
import RenderHelper

renderTriangles :: [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderTriangles = renderVerticesAs Triangles

renderTrianglesByIndices :: [(Int, Int, Int)] -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderTrianglesByIndices = renderVerticesBy3IndicesAs Triangles

renderTriangleByIndices :: (Int, Int, Int) -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderTriangleByIndices indices = renderVerticesBy3IndicesAs Triangles [indices]
