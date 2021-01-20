module RenderHelper where

import Graphics.UI.GLUT

renderVerticesAs :: PrimitiveMode -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderVerticesAs primitiveShape points = renderPrimitive primitiveShape $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) points

renderVerticesBy3IndicesAs :: PrimitiveMode -> [(Int, Int, Int)] -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderVerticesBy3IndicesAs primitiveShape indices points = renderPrimitive primitiveShape $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) (indices3ToVertices indices points)

renderVerticesBy4IndicesAs :: PrimitiveMode -> [(Int, Int, Int, Int)] -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderVerticesBy4IndicesAs primitiveShape indices points = renderPrimitive primitiveShape $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) (indices4ToVertices indices points)

renderVerticesBy5IndicesAs :: PrimitiveMode -> [(Int, Int, Int, Int, Int)] -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderVerticesBy5IndicesAs primitiveShape indices points = renderPrimitive primitiveShape $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) (indices5ToVertices indices points)

renderVerticesBy6IndicesAs :: PrimitiveMode -> [(Int, Int, Int, Int, Int, Int)] -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderVerticesBy6IndicesAs primitiveShape indices points = renderPrimitive primitiveShape $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) (indices6ToVertices indices points)

indices3ToVertices :: [(Int, Int, Int)] -> [(GLfloat, GLfloat, GLfloat)] -> [(GLfloat, GLfloat, GLfloat)]
indices3ToVertices indices vertices = concatMap (\(aInd, bInd, cInd) -> [vertices !! aInd, vertices !! bInd, vertices !! cInd]) indices

indices4ToVertices :: [(Int, Int, Int, Int)] -> [(GLfloat, GLfloat, GLfloat)] -> [(GLfloat, GLfloat, GLfloat)]
indices4ToVertices indices vertices = concatMap (\(aInd, bInd, cInd, dInd) -> [vertices !! aInd, vertices !! bInd, vertices !! cInd, vertices !! dInd]) indices

indices5ToVertices :: [(Int, Int, Int, Int, Int)] -> [(GLfloat, GLfloat, GLfloat)] -> [(GLfloat, GLfloat, GLfloat)]
indices5ToVertices indices vertices = concatMap (\(aInd, bInd, cInd, dInd, eInd) -> [vertices !! aInd, vertices !! bInd, vertices !! cInd, vertices !! dInd, vertices !! eInd]) indices

indices6ToVertices :: [(Int, Int, Int, Int, Int, Int)] -> [(GLfloat, GLfloat, GLfloat)] -> [(GLfloat, GLfloat, GLfloat)]
indices6ToVertices indices vertices = concatMap (\(aInd, bInd, cInd, dInd, eInd, fInd) -> [vertices !! aInd, vertices !! bInd, vertices !! cInd, vertices !! dInd, vertices !! eInd, vertices !! fInd]) indices