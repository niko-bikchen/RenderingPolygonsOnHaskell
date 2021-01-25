import Data.IORef
import Graphics.UI.GLUT
import Hexagon
import OrbitPointOfView
import Triangle

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (3.0, 1.0, 1.0), -- A.0
    (1.0, 3.0, 1.0), -- B.1
    (1.0, 1.0, 3.0), -- C.2
    (-3.0, -1.0, 1.0), -- D.3
    (-1.0, -3.0, 1.0), -- E.4
    (-1.0, -1.0, 3.0), -- F.5
    (-3.0, 1.0, -1.0), -- G.6
    (-1.0, 3.0, -1.0), -- H.7
    (-1.0, 1.0, -3.0), -- I.8
    (3.0, -1.0, -1.0), -- J.9
    (1.0, -3.0, -1.0), -- K.10
    (1.0, -1.0, -3.0) -- L.11
  ]

colors :: [Color3 GLfloat]
colors =
  [ Color3 1.0 1.0 0.0,
    Color3 0.0 0.0 1.0,
    Color3 0.5 0.35 0.05,
    Color3 1.0 0.0 0.0
  ]

hexagonIndices :: [(Int, Int, Int, Int, Int, Int)]
hexagonIndices =
  [ (0, 2, 5, 4, 10, 9),
    (3, 5, 2, 1, 7, 6),
    (10, 4, 3, 6, 8, 11),
    (7, 1, 0, 9, 11, 8)
  ]

triangleIndices :: [(Int, Int, Int)]
triangleIndices =
  [ (1, 2, 0),
    (4, 5, 3),
    (11, 9, 10),
    (8, 6, 7)
  ]

keyboard :: IORef (Int, Int, GLdouble) -> KeyboardMouseCallback
keyboard pPos c _ _ _ = keyForPos pPos c

display :: IORef (Int, Int, GLdouble) -> DisplayCallback
display pPos = do
  loadIdentity
  setPointOfView pPos

  clear [ColorBuffer, DepthBuffer]
  cullFace $= Just Back

  rotate 120 $ Vector3 0.0 (1.0 :: GLfloat) 0.0

  color $ colors !! 0
  renderHexagonByIndices (hexagonIndices !! 0) vertices
  color $ colors !! 1
  renderHexagonByIndices (hexagonIndices !! 1) vertices
  color $ colors !! 2
  renderHexagonByIndices (hexagonIndices !! 2) vertices
  color $ colors !! 3
  renderHexagonByIndices (hexagonIndices !! 3) vertices

  color $ colors !! 2
  renderTriangleByIndices (triangleIndices !! 0) vertices
  color $ colors !! 3
  renderTriangleByIndices (triangleIndices !! 1) vertices
  color $ colors !! 1
  renderTriangleByIndices (triangleIndices !! 2) vertices
  color $ colors !! 0
  renderTriangleByIndices (triangleIndices !! 3) vertices

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