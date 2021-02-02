module RenderHelper where

import Data.IORef
import Graphics.UI.GLUT
import OrbitPointOfView

data MyPoint = MyPoint
  { xC :: GLfloat,
    yC :: GLfloat,
    zC :: GLfloat
  }
  deriving (Show)

data PolyFace = PolyFace
  { polyFaceIndices :: [Int],
    polyFaceColor :: Color3 GLfloat
  }

red, blue, green, yellow, brown, orange, white :: Color3 GLfloat
red = Color3 1.0 0.0 0.0
blue = Color3 0.0 0.0 1.0
green = Color3 0.0 1.0 0.0
yellow = Color3 1.0 1.0 0.0
brown = Color3 0.5 0.35 0.05
orange = Color3 1.0 0.65 0.0
white = Color3 1.0 1.0 1.0

normilizeVector :: Normal3 GLfloat -> Normal3 GLfloat
normilizeVector (Normal3 x y z) = Normal3 (x / magnitude) (y / magnitude) (z / magnitude)
  where
    magnitude = sqrt ((x * x) + (y * y) + (z * z))

tupleToPoint :: (GLfloat, GLfloat, GLfloat) -> MyPoint
tupleToPoint (x, y, z) = MyPoint {xC = x, yC = y, zC = z}

makePointPairs :: [(GLfloat, GLfloat, GLfloat)] -> Int -> [(MyPoint, MyPoint)] -> [(MyPoint, MyPoint)]
makePointPairs vertices currentArrayIndex result
  | currentArrayIndex >= length vertices = result
  | otherwise = makePointPairs vertices (currentArrayIndex + 1) (result ++ [(current, next)])
  where
    current = tupleToPoint $ vertices !! currentArrayIndex
    next = tupleToPoint $ vertices !! mod (currentArrayIndex + 1) (length vertices)

calculateSurfaceNormall :: [(GLfloat, GLfloat, GLfloat)] -> Normal3 GLfloat
calculateSurfaceNormall points = normilizeVector (Normal3 nX nY nZ)
  where
    pointPairs = makePointPairs points 0 []
    nX = foldl (\res (current, next) -> res + ((yC current - yC next) * (zC current + zC next))) 0.0 pointPairs
    nY = foldl (\res (current, next) -> res + ((zC current - zC next) * (xC current + xC next))) 0.0 pointPairs
    nZ = foldl (\res (current, next) -> res + ((xC current - xC next) * (yC current + yC next))) 0.0 pointPairs

polyIndicesToVertices :: [Int] -> [(GLfloat, GLfloat, GLfloat)] -> [(GLfloat, GLfloat, GLfloat)]
polyIndicesToVertices indices vertices = map (vertices !!) indices

renderPolyFace :: PolyFace -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderPolyFace (PolyFace faceIndices faceColor) vertices = renderPrimitive TriangleFan $
  do
    color faceColor
    let faceVertices = polyIndicesToVertices faceIndices vertices
    mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) faceVertices

renderShadowedPolyFace :: PolyFace -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderShadowedPolyFace (PolyFace faceIndices faceColor) vertices = renderPrimitive TriangleFan $
  do
    color faceColor
    let faceVertices = polyIndicesToVertices faceIndices vertices
    let faceNormal = calculateSurfaceNormall faceVertices
    normal faceNormal
    mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) faceVertices

makeSimilarFaces :: [[Int]] -> Color3 GLfloat -> [PolyFace]
makeSimilarFaces indices color = map (`PolyFace` color) indices

renderPolyFaces :: [PolyFace] -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderPolyFaces polyFaces vertices = mapM_ (`renderPolyFace` vertices) polyFaces

renderShadowedPolyFaces :: [PolyFace] -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderShadowedPolyFaces polyFaces vertices = mapM_ (`renderShadowedPolyFace` vertices) polyFaces