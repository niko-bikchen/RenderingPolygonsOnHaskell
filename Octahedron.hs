import Data.IORef
import Graphics.UI.GLUT
import OrbitPointOfView
import Triangle

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (1.0, 0.0, 0.0),
    (-1.0, 0.0, 0.0),
    (0.0, 1.0, 0.0),
    (0.0, -1.0, 0.0),
    (0.0, 0.0, 1.0),
    (0.0, 0.0, -1.0)
  ]

colors :: [Color3 GLfloat]
colors =
  [ Color3 1.0 0.0 0.0,
    Color3 0.0 0.0 1.0,
    Color3 1.0 1.0 0.0,
    Color3 0.5 0.35 0.05
  ]

indices :: [(Int, Int, Int)]
indices =
  [ (4, 3, 0),
    (4, 0, 2),
    (4, 2, 1),
    (4, 1, 3),
    (0, 3, 5),
    (3, 1, 5),
    (1, 2, 5),
    (2, 0, 5)
  ]

keyboard :: IORef (Int, Int, GLdouble) -> KeyboardMouseCallback
keyboard pPos c _ _ _ = keyForPos pPos c

display :: IORef (Int, Int, GLdouble) -> DisplayCallback
display pPos = do
  loadIdentity
  setPointOfView pPos

  clear [ColorBuffer, DepthBuffer]
  cullFace $= Just Back

  rotate 90 $ Vector3 (1.0 :: GLfloat) 0.0 0.0

  color $ colors !! 0
  renderTriangleByIndices (indices !! 0) vertices
  color $ colors !! 1
  renderTriangleByIndices (indices !! 1) vertices
  color $ colors !! 2
  renderTriangleByIndices (indices !! 2) vertices
  color $ colors !! 3
  renderTriangleByIndices (indices !! 3) vertices

  renderTriangleByIndices (indices !! 4) vertices
  color $ colors !! 2
  renderTriangleByIndices (indices !! 5) vertices
  color $ colors !! 1
  renderTriangleByIndices (indices !! 6) vertices
  color $ colors !! 0
  renderTriangleByIndices (indices !! 7) vertices

  swapBuffers

main :: IO ()
main = do
  (progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, RGBMode, DoubleBuffered]
  initialWindowSize $= Size 500 500
  initialWindowPosition $= Position 0 0
  _ <- createWindow progName

  pPos <- newIORef (90, 270, 4)

  clearColor $= Color4 0 0 0 0
  displayCallback $= display pPos

  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (keyboard pPos)

  mainLoop