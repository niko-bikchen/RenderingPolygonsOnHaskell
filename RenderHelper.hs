module RenderHelper where

import Graphics.UI.GLUT

renderVerticesAs :: PrimitiveMode -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderVerticesAs primitiveShape points = renderPrimitive primitiveShape $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) points

renderVerticesByIndicesAs :: PrimitiveMode -> [(Int, Int, Int)] -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderVerticesByIndicesAs primitiveShape indices points = renderPrimitive primitiveShape $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) (indicesToVertices indices points)

indicesToVertices :: [(Int, Int, Int)] -> [(GLfloat, GLfloat, GLfloat)] -> [(GLfloat, GLfloat, GLfloat)]
indicesToVertices indices vertices = concatMap (\(xInd, yInd, zInd) -> [vertices !! xInd, vertices !! yInd, vertices !! zInd]) indices