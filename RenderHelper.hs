module RenderHelper where

import Data.IORef
import Data.List
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
  deriving (Show)

red, blue, green, yellow, brown, orange, white, black, pink :: Color3 GLfloat
red = Color3 1.0 0.0 0.0
blue = Color3 0.0 0.0 1.0
green = Color3 0.0 1.0 0.0
yellow = Color3 1.0 1.0 0.0
brown = Color3 0.5 0.35 0.05
orange = Color3 1.0 0.65 0.0
white = Color3 1.0 1.0 1.0
black = Color3 0.0 0.0 0.0
pink = Color3 1.000 0.078 0.576

intersectLists :: [Int] -> [Int] -> [Int]
intersectLists (x : xs) ys =
  if x `elem` ys
    then x : intersectLists xs (delete x ys)
    else intersectLists xs ys
intersectLists [] _ = []

matchPolygonsToBase :: [Int] -> [[Int]] -> [[Int]]
matchPolygonsToBase base [polygon]
  | commonPoints >= 2 = [polygon, base]
  | otherwise = [base]
  where
    commonPoints = length $ intersectLists base polygon
matchPolygonsToBase base (polygon : polygons)
  | commonPoints >= 2 = polygon : matchPolygonsToBase base polygons
  | otherwise = matchPolygonsToBase base polygons
  where
    commonPoints = length $ intersectLists base polygon

matchPolygonsToBases :: [[Int]] -> [[Int]] -> [[[Int]]]
matchPolygonsToBases bases polygons = map (`matchPolygonsToBase` polygons) bases

findPolygonsWithoutBase :: [[Int]] -> [[Int]] -> [[Int]]
findPolygonsWithoutBase bases [polygon]
  | all (\el -> length el <= 1) commonPoints = [polygon]
  | otherwise = []
  where
    commonPoints = map (`intersectLists` polygon) bases
findPolygonsWithoutBase bases (polygon : polygons)
  | all (\el -> length el <= 1) commonPoints = polygon : findPolygonsWithoutBase bases polygons
  | otherwise = findPolygonsWithoutBase bases polygons
  where
    commonPoints = map (`intersectLists` polygon) bases

makeBatch :: [[Int]] -> [(GLfloat, GLfloat, GLfloat)] -> Color3 GLfloat -> Color3 GLfloat -> [PolyFace]
makeBatch [polygon] vertices mainColor secondaryColor = [PolyFace polygon mainColor]
makeBatch (polygon : batch) vertices mainColor secondaryColor = PolyFace polygon secondaryColor : makeBatch batch vertices mainColor secondaryColor

makeBatches :: [[[Int]]] -> [(GLfloat, GLfloat, GLfloat)] -> Color3 GLfloat -> Color3 GLfloat -> [[PolyFace]]
makeBatches batches vertices mainColor secondaryColor = map (\batch -> makeBatch batch vertices mainColor secondaryColor) batches

normalizeVector :: Vector3 GLfloat -> Vector3 GLfloat
normalizeVector (Vector3 x y z) = Vector3 (x / magnitude) (y / magnitude) (z / magnitude)
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

calculateSurfaceNormall :: [(GLfloat, GLfloat, GLfloat)] -> Vector3 GLfloat
calculateSurfaceNormall points = normalizeVector (Vector3 nX nY nZ)
  where
    pointPairs = makePointPairs points 0 []
    nX = foldl (\res (current, next) -> res + ((yC current - yC next) * (zC current + zC next))) 0.0 pointPairs
    nY = foldl (\res (current, next) -> res + ((zC current - zC next) * (xC current + xC next))) 0.0 pointPairs
    nZ = foldl (\res (current, next) -> res + ((xC current - xC next) * (yC current + yC next))) 0.0 pointPairs

polyIndicesToVertices :: [Int] -> [(GLfloat, GLfloat, GLfloat)] -> [(GLfloat, GLfloat, GLfloat)]
polyIndicesToVertices indices vertices = map (vertices !!) indices

renderShadowedPolyFace :: PolyFace -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderShadowedPolyFace (PolyFace faceIndices faceColor) vertices = renderPrimitive Polygon $
  do
    color faceColor
    let faceVertices = polyIndicesToVertices faceIndices vertices
    let (Vector3 nX nY nZ) = calculateSurfaceNormall faceVertices
    normal (Normal3 nX nY nZ)
    mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) faceVertices

makeSimilarFaces :: [[Int]] -> Color3 GLfloat -> [PolyFace]
makeSimilarFaces indices color = map (`PolyFace` color) indices

renderShadowedPolyFaces :: [PolyFace] -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderShadowedPolyFaces polyFaces vertices = mapM_ (`renderShadowedPolyFace` vertices) polyFaces

renderShadowedSpike :: PolyFace -> [(GLfloat, GLfloat, GLfloat)] -> GLfloat -> IO ()
renderShadowedSpike (PolyFace faceIndices faceColor) vertices spikeHeight = do
  renderPrimitive Triangles $
    do
      color faceColor
      let faceVertices = polyIndicesToVertices faceIndices vertices
      let faceNormal = calculateSurfaceNormall faceVertices
      let pointPairs = makePointPairs faceVertices 0 []
      let faceNormals = replicate (length pointPairs) faceNormal
      let spikeSides = zip pointPairs faceNormals
      mapM_
        ( \((MyPoint aX aY aZ, MyPoint bX bY bZ), Vector3 nX nY nZ) ->
            do
              let (Vector3 nnX nnY nnZ) = calculateSurfaceNormall [(aX, aY, aZ), (bX, bY, bZ), (nX, nY, nZ)]
              normal (Normal3 nnX nnY nnZ)
              vertex $ Vertex3 aX aY aZ
              vertex $ Vertex3 bX bY bZ
              vertex $ Vertex3 (spikeHeight * nX) (spikeHeight * nY) (spikeHeight * nZ)
        )
        spikeSides

renderShadowedSpikes :: [PolyFace] -> [(GLfloat, GLfloat, GLfloat)] -> GLfloat -> IO ()
renderShadowedSpikes polyFaces vertices spikeHeight = mapM_ (\face -> renderShadowedSpike face vertices spikeHeight) polyFaces

renderPolygonBoundary :: IO () -> IO ()
renderPolygonBoundary renderPolygonFrame = do
  scale 1.0053 1.0053 (1.0053 :: GLfloat)
  renderPolygonFrame

subtractVectors :: Vector3 GLfloat -> Vector3 GLfloat -> Vector3 GLfloat
subtractVectors (Vector3 aX aY aZ) (Vector3 bX bY bZ) = Vector3 (aX - bX) (aY - bY) (aZ - bZ)

vectorCrossProduct :: Vector3 GLfloat -> Vector3 GLfloat -> Vector3 GLfloat
vectorCrossProduct (Vector3 aX aY aZ) (Vector3 bX bY bZ) = Vector3 rX rY rZ
  where
    rX = aY * bZ - aZ * bY
    rY = aZ * bX - aX * bZ
    rZ = aX * bY - aY * bX

vectorDotProduct :: Vector3 GLfloat -> Vector3 GLfloat -> GLfloat
vectorDotProduct (Vector3 aX aY aZ) (Vector3 bX bY bZ) = aX * bX + aY * bY + aZ * bZ

divideVectorByScalar :: Vector3 GLfloat -> GLfloat -> Vector3 GLfloat
divideVectorByScalar (Vector3 aX aY aZ) scalar = Vector3 (aX / scalar) (aY / scalar) (aZ / scalar)

projectOntoSurface :: (GLfloat, GLfloat) -> Vector3 GLfloat -> Vector3 GLfloat
projectOntoSurface (width, height) touchPoint
  | lengthSq <= radiusSq = normalizeVector $ Vector3 centerdX (- centerdY) (sqrt (radiusSq - lengthSq))
  | otherwise = normalizeVector $ Vector3 (centerdX * (radius / sqrt lengthSq)) ((- centerdY) * (radius / sqrt lengthSq)) 0.0
  where
    radius = width / 3
    center = Vector3 (width / 2) (height / 2) 0.0
    (Vector3 centerdX centerdY centerdZ) = subtractVectors touchPoint center
    radiusSq = radius * radius
    lengthSq = centerdX * centerdX + centerdY * centerdY