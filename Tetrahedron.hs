import Control.Monad (unless)
import Data.IORef
import Data.List (genericLength)
import Foreign (Ptr, Storable, newArray)
import Graphics.UI.GLUT
import OrbitPointOfView
import Triangle

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (1.0, 1.0, 1.0), -- Vertex A
    (-1.0, 1.0, -1.0), -- Vertex B
    (1.0, -1.0, -1.0), -- Vertex C
    (-1.0, -1.0, 1.0) -- Vertex D
  ]

colors :: [Color3 GLfloat]
colors =
  [ Color3 1.0 0.0 0.0, -- Side 1
    Color3 0.0 1.0 0.0, -- Side 2
    Color3 0.0 0.0 1.0, -- Side 3
    Color3 1.0 1.0 0.0, -- Side 4
    Color3 1.0 1.0 1.0
  ]

indices :: [(Int, Int, Int)]
indices =
  [ (3, 1, 2),
    (0, 3, 2),
    (0, 1, 3),
    (0, 2, 1)
  ]

keyboard :: IORef (Int, Int, GLdouble) -> KeyboardMouseCallback
keyboard pPos c _ _ _ = keyForPos pPos c

display :: IORef (Int, Int, GLdouble) -> DisplayCallback
display pPos = do
  loadIdentity
  setPointOfView pPos

  clear [ColorBuffer, DepthBuffer]
  cullFace $= Just Back

  color $ colors !! 0
  renderTriangleByIndices (indices !! 0) vertices
  color $ colors !! 1
  renderTriangleByIndices (indices !! 1) vertices
  color $ colors !! 2
  renderTriangleByIndices (indices !! 2) vertices
  color $ colors !! 3
  renderTriangleByIndices (indices !! 3) vertices

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