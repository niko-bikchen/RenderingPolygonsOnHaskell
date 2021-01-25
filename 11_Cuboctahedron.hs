import Data.IORef
import Graphics.UI.GLUT
import OrbitPointOfView
import RenderHelper
import Square
import Triangle

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (0.0, 0.0, 1.154701),
    (1.0, 0.0, 0.5773503),
    (0.3333333, 0.942809, 0.5773503),
    (-1.0, 0.0, 0.5773503),
    (-0.3333333, -0.942809, 0.5773503),
    (1.0, 0.0, -0.5773503),
    (0.6666667, -0.942809, 0.0),
    (-0.6666667, 0.942809, 0.0),
    (0.3333333, 0.942809, -0.5773503),
    (-1.0, 0.0, -0.5773503),
    (-0.3333333, -0.942809, -0.5773503),
    (0.0, 0.0, -1.154701)
  ]

squareIndices :: [(Int, Int, Int, Int)]
squareIndices =
  [ (0, 2, 7, 3),
    (0, 4, 6, 1),
    (1, 5, 8, 2),
    (3, 9, 10, 4),
    (5, 6, 10, 11),
    (7, 8, 11, 9)
  ]

triangleIndices :: [(Int, Int, Int)]
triangleIndices =
  [ (0, 1, 2),
    (0, 3, 4),
    (1, 6, 5),
    (2, 8, 7),
    (3, 7, 9),
    (4, 10, 6),
    (5, 11, 8),
    (9, 11, 10)
  ]

keyboard :: IORef (Int, Int, GLdouble) -> KeyboardMouseCallback
keyboard pPos c _ _ _ = keyForPos pPos c

display :: IORef (Int, Int, GLdouble) -> DisplayCallback
display pPos = do
  loadIdentity
  setPointOfView pPos

  clear [ColorBuffer, DepthBuffer]
  cullFace $= Just Back

  rotate 140 $ Vector3 0.0 (1.0 :: GLfloat) 0.0

  color red
  renderTrianglesByIndices triangleIndices vertices

  color yellow
  renderSquareByIndices (squareIndices !! 0) vertices
  color blue
  renderSquareByIndices (squareIndices !! 1) vertices
  color brown
  renderSquareByIndices (squareIndices !! 2) vertices

  color brown
  renderSquareByIndices (squareIndices !! 3) vertices

  color yellow
  renderSquareByIndices (squareIndices !! 4) vertices

  color blue
  renderSquareByIndices (squareIndices !! 5) vertices

  swapBuffers

main :: IO ()
main = do
  (progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, RGBMode, DoubleBuffered]
  initialWindowSize $= Size 500 500
  initialWindowPosition $= Position 0 0
  _ <- createWindow progName

  pPos <- newIORef (90, 270, 10)

  clearColor $= Color4 0 0 0 0
  displayCallback $= display pPos

  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (keyboard pPos)

  mainLoop