import Control.Monad (unless)
import Data.IORef
import Data.List (genericLength)
import Foreign (Ptr, Storable, newArray)
import Graphics.UI.GLUT
import OrbitPointOfView
import Pentagon

coordX :: GLfloat
coordX = 0.525731112119133606

coordZ :: GLfloat
coordZ = 0.850650808352039932

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (1.0, 1.0, 1.0), -- A.0
    (1.0, 1.0, -1.0), -- B.1
    (1.0, -1.0, 1.0), -- C.2
    (1.0, -1.0, -1.0), -- D.3
    (-1.0, 1.0, 1.0), -- E.4
    (-1.0, 1.0, -1.0), -- F.5
    (-1.0, -1.0, 1.0), -- G.6
    (-1.0, -1.0, -1.0), -- H.7
    (0.0, 1.618, 0.618), -- I.8
    (0.0, 1.618, -0.618), -- J.9
    (0.0, -1.618, 0.618), -- K.10
    (0.0, -1.618, -0.618), -- L.11
    (0.618, 0.0, 1.618), -- M.12
    (0.618, 0.0, -1.618), -- N.13
    (-0.618, 0.0, 1.618), -- O.14
    (-0.618, 0.0, -1.618), -- P.15
    (1.618, 0.618, 0.0), -- Q.16
    (1.618, -0.618, 0.0), -- R.17
    (-1.618, 0.618, 0.0), -- S.18
    (-1.618, -0.618, 0.0) -- T.19
  ]

colors :: [Color3 GLfloat]
colors =
  [ Color3 1.0 1.0 0.0,
    Color3 0.0 0.0 1.0,
    Color3 0.5 0.35 0.05,
    Color3 1.0 0.0 0.0
  ]

indices :: [(Int, Int, Int, Int, Int)]
indices =
  [ (2, 12, 14, 6, 10),
    (8, 4, 14, 12, 0),
    (19, 6, 14, 4, 18),
    (16, 0, 12, 2, 17),
    (11, 10, 6, 19, 7),
    (3, 17, 2, 10, 11),
    (13, 3, 11, 7, 15),
    (15, 7, 19, 18, 5),
    (1, 16, 17, 3, 13),
    (9, 8, 0, 16, 1),
    (5, 18, 4, 8, 9),
    (15, 5, 9, 1, 13)
  ]

keyboard :: IORef (Int, Int, GLdouble) -> KeyboardMouseCallback
keyboard pPos c _ _ _ = keyForPos pPos c

display :: IORef (Int, Int, GLdouble) -> DisplayCallback
display pPos = do
  loadIdentity
  setPointOfView pPos

  clear [ColorBuffer, DepthBuffer]
  cullFace $= Just Back

  rotate 180 $ Vector3 0.0 (1.0 :: GLfloat) 0.0

  -- Top "hat"
  color $ colors !! 3
  renderPentagonByIndices (indices !! 0) vertices
  color $ colors !! 0
  renderPentagonByIndices (indices !! 1) vertices
  color $ colors !! 1
  renderPentagonByIndices (indices !! 2) vertices
  color $ colors !! 2
  renderPentagonByIndices (indices !! 3) vertices
  renderPentagonByIndices (indices !! 4) vertices
  color $ colors !! 1
  renderPentagonByIndices (indices !! 5) vertices -- Blue between two browns

  -- Bottom "hat"
  color $ colors !! 3
  renderPentagonByIndices (indices !! 6) vertices
  color $ colors !! 0
  renderPentagonByIndices (indices !! 7) vertices
  renderPentagonByIndices (indices !! 8) vertices
  color $ colors !! 1
  renderPentagonByIndices (indices !! 9) vertices
  color $ colors !! 3
  renderPentagonByIndices (indices !! 10) vertices
  color $ colors !! 2
  renderPentagonByIndices (indices !! 11) vertices

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