import Data.IORef
import Graphics.UI.GLUT
import Hexagon
import OrbitPointOfView
import Square

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (0, 1, 2), -- A.0
    (0, -1, 2), -- B.1
    (0, 1, -2), -- C.2
    (0, -1, -2), -- D.3
    --
    (0, 2, 1), -- E.4
    (2, 1, 0), -- F.5
    (1, 2, 0), -- G.6
    (1, 0, 2), -- H.7
    (2, 0, 1), -- I.8
    --
    (0, 2, -1), -- J.9
    (2, -1, 0), -- K.10
    (-1, 2, 0), -- L.11
    (-1, 0, 2), -- M.12
    (2, 0, -1), -- N.13
    --
    (0, -2, 1), -- O.14
    (-2, 1, 0), -- P.15
    (1, -2, 0), -- Q.16
    (1, 0, -2), -- R.17
    (-2, 0, 1), -- S.18
    --
    (0, -2, -1), -- T.19
    (-2, -1, 0), -- U.20
    (-1, -2, 0), -- V.21
    (-1, 0, -2), -- W.22
    (-2, 0, -1) -- Z.23
  ]

colors :: [Color3 GLfloat]
colors =
  [ Color3 1.0 1.0 0.0,
    Color3 0.0 0.0 1.0,
    Color3 0.5 0.35 0.05,
    Color3 1.0 0.0 0.0,
    Color3 0.0 1.0 0.0
  ]

hexagonIndices :: [(Int, Int, Int, Int, Int, Int)]
hexagonIndices =
  [ (4, 0, 7, 8, 5, 6),
    (9, 6, 5, 13, 17, 2),
    (12, 0, 4, 11, 15, 18),
    (8, 7, 1, 14, 16, 10),
    (19, 21, 20, 23, 22, 3),
    (13, 10, 16, 19, 3, 17),
    (14, 1, 12, 18, 20, 21),
    (23, 15, 11, 9, 2, 22)
  ]

squareIndices :: [(Int, Int, Int, Int)]
squareIndices =
  [ (11, 4, 6, 9),
    (1, 7, 0, 12),
    (13, 5, 8, 10),
    (16, 14, 21, 19),
    (17, 3, 22, 2),
    (20, 18, 15, 23)
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
  color $ colors !! 4
  renderSquareByIndices (squareIndices !! 0) vertices
  renderSquareByIndices (squareIndices !! 1) vertices
  renderSquareByIndices (squareIndices !! 2) vertices

  color $ colors !! 0
  renderHexagonByIndices (hexagonIndices !! 4) vertices
  color $ colors !! 2
  renderHexagonByIndices (hexagonIndices !! 5) vertices
  color $ colors !! 1
  renderHexagonByIndices (hexagonIndices !! 6) vertices
  color $ colors !! 3
  renderHexagonByIndices (hexagonIndices !! 7) vertices
  color $ colors !! 4
  renderSquareByIndices (squareIndices !! 3) vertices
  renderSquareByIndices (squareIndices !! 4) vertices
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