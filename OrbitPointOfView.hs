module OrbitPointOfView where

import Data.IORef
import Graphics.UI.GLUT

setPointOfView :: IORef (Int, Int, GLdouble) -> IO ()
setPointOfView pPos = do
  (alpha, beta, r) <- get pPos
  let (x, y, z) = calculatePointOfView alpha beta r
      (x2, y2, z2) = calculatePointOfView ((alpha + 90) `mod` 360) beta r
  lookAt (Vertex3 x y z) (Vertex3 0 0 0) (Vector3 x2 y2 z2)

calculatePointOfView :: Int -> Int -> GLdouble -> (GLdouble, GLdouble, GLdouble)
calculatePointOfView alp bet r =
  let alpha = fromIntegral alp * 2 * pi / 360
      beta = fromIntegral bet * 2 * pi / 360
      y = r * cos alpha
      u = r * sin alpha
      x = u * cos beta
      z = u * sin beta
   in (x, y, z)

keyForPos :: IORef (Int, Int, GLdouble) -> Key -> IO ()
keyForPos pPos (Char '+') = modPos pPos (id, id, \x -> x -0.1)
keyForPos pPos (Char '-') = modPos pPos (id, id, (+) 0.1)
keyForPos pPos (SpecialKey KeyLeft) = modPos pPos (id, (+) 359, id)
keyForPos pPos (SpecialKey KeyRight) = modPos pPos (id, (+) 1, id)
keyForPos pPos (SpecialKey KeyUp) = modPos pPos ((+) 1, id, id)
keyForPos pPos (SpecialKey KeyDown) = modPos pPos ((+) 359, id, id)
keyForPos _ _ = return ()

modPos :: IORef (Int, Int, GLdouble) -> (Int -> Int, Int -> Int, GLdouble -> GLdouble) -> IO ()
modPos pPos (ffst, fsnd, ftrd) = do
  (alpha, beta, r) <- get pPos
  pPos $= (ffst alpha `mod` 360, fsnd beta `mod` 360, ftrd r)
  postRedisplay Nothing

reshape :: ReshapeCallback
reshape s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  let near = 0.05
      far = 10
      fov = 45
      ang = (fov * pi) / (360)
      top = near / (cos (ang) / sin (ang))
      aspect = fromIntegral (w) / fromIntegral (h)
      right = top * aspect
  frustum (- right) right (- top) top near far
  matrixMode $= Modelview 0