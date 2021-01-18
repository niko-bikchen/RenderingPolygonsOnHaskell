import Control.Monad (unless)
import Data.IORef
import Data.List (genericLength)
import Foreign (Ptr, Storable, newArray)
import Graphics.UI.GLUT
import OrbitPointOfView
import Triangle

coordX :: GLfloat
coordX = 0.525731112119133606

coordZ :: GLfloat
coordZ = 0.850650808352039932

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (- coordX, 0.0, coordZ),
    (coordX, 0.0, coordZ),
    (- coordX, 0.0, - coordZ),
    (coordX, 0.0, - coordZ),
    (0.0, coordZ, coordX),
    (0.0, coordZ, - coordX),
    (0.0, - coordZ, coordX),
    (0.0, - coordZ, - coordX),
    (coordZ, coordX, 0.0),
    (- coordZ, coordX, 0.0),
    (coordZ, - coordX, 0.0),
    (- coordZ, - coordX, 0.0)
  ]

colors :: [Color3 GLfloat]
colors =
  [ Color3 1.0 1.0 0.0,
    Color3 0.0 0.0 1.0,
    Color3 0.5 0.35 0.05,
    Color3 1.0 0.0 0.0,
    Color3 0.0 1.0 0.0
  ]

indices :: [(Int, Int, Int)]
indices =
  [ (0, 4, 1),
    (0, 9, 4),
    (9, 5, 4),
    (4, 5, 8),
    (4, 8, 1),
    (8, 10, 1),
    (8, 3, 10),
    (5, 3, 8),
    (5, 2, 3),
    (2, 7, 3),
    (7, 10, 3),
    (7, 6, 10),
    (7, 11, 6),
    (11, 0, 6),
    (0, 1, 6),
    (6, 1, 10),
    (9, 0, 11),
    (9, 11, 2),
    (9, 2, 5),
    (7, 2, 11)
  ]

keyboard :: IORef (Int, Int, GLdouble) -> KeyboardMouseCallback
keyboard pPos c _ _ _ = keyForPos pPos c

display :: IORef (Int, Int, GLdouble) -> DisplayCallback
display pPos = do
  loadIdentity
  setPointOfView pPos

  clear [ColorBuffer, DepthBuffer]
  cullFace $= Just Back

  rotate 120 $ Vector3 (1.0 :: GLfloat) 0.0 0.0

  -- Top "hat"
  color $ colors !! 0
  renderTriangleByIndices (indices !! 0) vertices
  color $ colors !! 1
  renderTriangleByIndices (indices !! 1) vertices
  color $ colors !! 2
  renderTriangleByIndices (indices !! 2) vertices
  color $ colors !! 3
  renderTriangleByIndices (indices !! 3) vertices
  color $ colors !! 4
  renderTriangleByIndices (indices !! 4) vertices

  -- Triangles which come from top
  color $ colors !! 2
  renderTriangleByIndices (indices !! 5) vertices
  color $ colors !! 1
  renderTriangleByIndices (indices !! 7) vertices
  color $ colors !! 3
  renderTriangleByIndices (indices !! 14) vertices
  color $ colors !! 4
  renderTriangleByIndices (indices !! 16) vertices
  color $ colors !! 0
  renderTriangleByIndices (indices !! 18) vertices

  -- Bottom "hat"
  color $ colors !! 1
  renderTriangleByIndices (indices !! 19) vertices
  color $ colors !! 2
  renderTriangleByIndices (indices !! 9) vertices
  color $ colors !! 3
  renderTriangleByIndices (indices !! 10) vertices
  color $ colors !! 4
  renderTriangleByIndices (indices !! 11) vertices
  color $ colors !! 0
  renderTriangleByIndices (indices !! 12) vertices

  -- Triangles which come from bottom
  renderTriangleByIndices (indices !! 6) vertices
  color $ colors !! 4
  renderTriangleByIndices (indices !! 8) vertices
  color $ colors !! 2
  renderTriangleByIndices (indices !! 13) vertices
  color $ colors !! 1
  renderTriangleByIndices (indices !! 15) vertices
  color $ colors !! 3
  renderTriangleByIndices (indices !! 17) vertices

  swapBuffers

main :: IO ()
main = do
  (progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, RGBMode, DoubleBuffered]
  initialWindowSize $= Size 500 500
  initialWindowPosition $= Position 0 0
  _ <- createWindow progName

  pPos <- newIORef (90, 270, 5)

  clearColor $= Color4 0 0 0 0
  displayCallback $= display pPos

  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (keyboard pPos)

  mainLoop