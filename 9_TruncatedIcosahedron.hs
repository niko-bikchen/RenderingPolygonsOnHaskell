import Data.IORef
import Graphics.UI.GLUT
import Hexagon
import OrbitPointOfView
import Pentagon

vertices :: [(GLfloat, GLfloat, GLfloat)]
vertices =
  [ (0.0, 0.0, 1.021),
    (0.4035482, 0.0, 0.9378643),
    (-0.2274644, 0.3333333, 0.9378643),
    (-0.1471226, -0.375774, 0.9378643),
    (0.579632, 0.3333333, 0.7715933),
    (0.5058321, -0.375774, 0.8033483),
    (-0.6020514, 0.2908927, 0.7715933),
    (-0.05138057, 0.6666667, 0.7715933),
    (0.1654988, -0.6080151, 0.8033483),
    (-0.5217096, -0.4182147, 0.7715933),
    (0.8579998, 0.2908927, 0.4708062),
    (0.3521676, 0.6666667, 0.6884578),
    (0.7841999, -0.4182147, 0.5025612),
    (-0.657475, 0.5979962, 0.5025612),
    (-0.749174, -0.08488134, 0.6884578),
    (-0.3171418, 0.8302373, 0.5025612),
    (0.1035333, -0.8826969, 0.5025612),
    (-0.5836751, -0.6928964, 0.4708062),
    (0.8025761, 0.5979962, 0.2017741),
    (0.9602837, -0.08488134, 0.3362902),
    (0.4899547, 0.8302373, 0.3362902),
    (0.7222343, -0.6928964, 0.2017741),
    (-0.8600213, 0.5293258, 0.1503935),
    (-0.9517203, -0.1535518, 0.3362902),
    (-0.1793548, 0.993808, 0.1503935),
    (0.381901, -0.9251375, 0.2017741),
    (-0.2710537, -0.9251375, 0.3362902),
    (-0.8494363, -0.5293258, 0.2017741),
    (0.8494363, 0.5293258, -0.2017741),
    (1.007144, -0.1535518, -0.06725804),
    (0.2241935, 0.993808, 0.06725804),
    (0.8600213, -0.5293258, -0.1503935),
    (-0.7222343, 0.6928964, -0.2017741),
    (-1.007144, 0.1535518, 0.06725804),
    (-0.381901, 0.9251375, -0.2017741),
    (0.1793548, -0.993808, -0.1503935),
    (-0.2241935, -0.993808, -0.06725804),
    (-0.8025761, -0.5979962, -0.2017741),
    (0.5836751, 0.6928964, -0.4708062),
    (0.9517203, 0.1535518, -0.3362902),
    (0.2710537, 0.9251375, -0.3362902),
    (0.657475, -0.5979962, -0.5025612),
    (-0.7841999, 0.4182147, -0.5025612),
    (-0.9602837, 0.08488134, -0.3362902),
    (-0.1035333, 0.8826969, -0.5025612),
    (0.3171418, -0.8302373, -0.5025612),
    (-0.4899547, -0.8302373, -0.3362902),
    (-0.8579998, -0.2908927, -0.4708062),
    (0.5217096, 0.4182147, -0.7715933),
    (0.749174, 0.08488134, -0.6884578),
    (0.6020514, -0.2908927, -0.7715933),
    (-0.5058321, 0.375774, -0.8033483),
    (-0.1654988, 0.6080151, -0.8033483),
    (0.05138057, -0.6666667, -0.7715933),
    (-0.3521676, -0.6666667, -0.6884578),
    (-0.579632, -0.3333333, -0.7715933),
    (0.1471226, 0.375774, -0.9378643),
    (0.2274644, -0.3333333, -0.9378643),
    (-0.4035482, 0.0, -0.9378643),
    (0.0, 0.0, -1.021)
  ]

colors :: [Color3 GLfloat]
colors =
  [ Color3 1.0 1.0 0.0,
    Color3 0.0 0.0 1.0,
    Color3 1.0 0.0 0.0,
    Color3 0.0 1.0 0.0,
    Color3 1.0 1.0 1.0,
    Color3 1.0 0.65 0.0
  ]

hexagonIndices :: [(Int, Int, Int, Int, Int, Int)]
hexagonIndices =
  [ (0, 1, 4, 11, 7, 2),
    (0, 2, 6, 14, 9, 3),
    (1, 5, 12, 19, 10, 4),
    (3, 9, 17, 26, 16, 8),
    (5, 8, 16, 25, 21, 12),
    (6, 13, 22, 33, 23, 14),
    (7, 11, 20, 30, 24, 15),
    (10, 19, 29, 39, 28, 18),
    (13, 15, 24, 34, 32, 22),
    (17, 27, 37, 46, 36, 26),
    (18, 28, 38, 40, 30, 20),
    (21, 25, 35, 45, 41, 31),
    (23, 33, 43, 47, 37, 27),
    (29, 31, 41, 50, 49, 39),
    (32, 34, 44, 52, 51, 42),
    (35, 36, 46, 54, 53, 45),
    (38, 48, 56, 52, 44, 40),
    (42, 51, 58, 55, 47, 43),
    (48, 49, 50, 57, 59, 56),
    (53, 54, 55, 58, 59, 57)
  ]

pentagonIndices :: [(Int, Int, Int, Int, Int)]
pentagonIndices =
  [ (0, 3, 8, 5, 1),
    (2, 7, 15, 13, 6),
    (4, 10, 18, 20, 11),
    (9, 14, 23, 27, 17),
    (12, 21, 31, 29, 19),
    (16, 26, 36, 35, 25),
    (22, 32, 42, 43, 33),
    (24, 30, 40, 44, 34),
    (28, 39, 49, 48, 38),
    (37, 47, 55, 54, 46),
    (41, 45, 53, 57, 50),
    (51, 52, 56, 59, 58)
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
  color $ colors !! 5
  renderHexagonByIndices (hexagonIndices !! 4) vertices

  color $ colors !! 5
  renderHexagonByIndices (hexagonIndices !! 5) vertices
  color $ colors !! 3
  renderHexagonByIndices (hexagonIndices !! 6) vertices
  color $ colors !! 2
  renderHexagonByIndices (hexagonIndices !! 8) vertices

  color $ colors !! 1
  renderHexagonByIndices (hexagonIndices !! 7) vertices
  color $ colors !! 2
  renderHexagonByIndices (hexagonIndices !! 9) vertices
  color $ colors !! 5
  renderHexagonByIndices (hexagonIndices !! 10) vertices

  color $ colors !! 0
  renderHexagonByIndices (hexagonIndices !! 11) vertices
  renderHexagonByIndices (hexagonIndices !! 12) vertices
  color $ colors !! 3
  renderHexagonByIndices (hexagonIndices !! 13) vertices
  color $ colors !! 1
  renderHexagonByIndices (hexagonIndices !! 14) vertices
  renderHexagonByIndices (hexagonIndices !! 15) vertices
  color $ colors !! 0
  renderHexagonByIndices (hexagonIndices !! 16) vertices
  color $ colors !! 3
  renderHexagonByIndices (hexagonIndices !! 17) vertices
  color $ colors !! 2
  renderHexagonByIndices (hexagonIndices !! 18) vertices
  color $ colors !! 5
  renderHexagonByIndices (hexagonIndices !! 19) vertices

  color $ colors !! 4
  mapM_ (\el -> renderPentagonByIndices el vertices) pentagonIndices

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