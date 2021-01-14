module Triangle where

import Graphics.UI.GLUT
import RenderHelper

renderTriangles :: [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderTriangles = renderVerticesAs Triangles

renderTrianglesByIndices :: [(Int, Int, Int)] -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderTrianglesByIndices = renderVerticesByIndicesAs Triangles

renderTriangleByIndices :: (Int, Int, Int) -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderTriangleByIndices indices = renderVerticesByIndicesAs Triangles [indices]
