import Control.Monad (unless)
import Data.IORef
import Data.List (genericLength)
import Foreign (Ptr, Storable, newArray)
import Graphics.UI.GLUT
import OrbitPointOfView
import Square

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (1.0, 1.0, -1.0),
    (-1.0, 1.0, -1.0),
    (-1.0, 1.0, 1.0),
    (1.0, 1.0, 1.0),
    (1.0, -1.0, 1.0),
    (-1.0, -1.0, 1.0),
    (-1.0, -1.0, -1.0),
    (1.0, -1.0, -1.0)
  ]

colors :: [Color3 GLfloat]
colors =
  [ Color3 0.0 0.0 1.0,
    Color3 1.0 1.0 0.0,
    Color3 0.5 0.35 0.05
  ]

indices :: [(Int, Int, Int, Int)]
indices =
  [ (0, 1, 2, 3),
    (4, 5, 6, 7),
    (3, 2, 5, 4),
    (7, 6, 1, 0),
    (2, 1, 6, 5),
    (0, 3, 4, 7)
  ]

keyboard :: IORef (Int, Int, GLdouble) -> KeyboardMouseCallback
keyboard pPos c _ _ _ = keyForPos pPos c

display :: IORef (Int, Int, GLdouble) -> DisplayCallback
display pPos = do
  loadIdentity
  setPointOfView pPos

  clear [ColorBuffer, DepthBuffer]
  cullFace $= Just Back

  color $ colors !! 1
  renderSquareByIndices (indices !! 0) vertices
  renderSquareByIndices (indices !! 1) vertices
  color $ colors !! 0
  renderSquareByIndices (indices !! 2) vertices
  renderSquareByIndices (indices !! 3) vertices
  color $ colors !! 2
  renderSquareByIndices (indices !! 4) vertices
  renderSquareByIndices (indices !! 5) vertices

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