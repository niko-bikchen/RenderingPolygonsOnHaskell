module Main where

import Data.Foldable
import Data.IORef
import Graphics.UI.GLUT
import Linear.Matrix
import Linear.Quaternion
import Linear.V3
import OrbitPointOfView
import P10_TruncatedDodecahedron
import P11_Cuboctahedron
import P12_Icosidodecahedron
import P13_Rhombicuboctahedron
import P14_Rhombicosidodecahedron
import P15_TruncatedCuboctahedron
import P16_TruncatedIcosidodecahedron
import P17_SnubCube
import P18_SnubDodecahedron
import P19_StellatedOctahedron
import P1_Tetrahedron
import P20_SmallStellatedDodecahedron
import P21_GreatDodecahedron
import P22_GreatStellatedDodecahedron
import P23_CubeAndOctahedron
import P24_GreatIcosahedron
import P2_Octahedron
import P3_Cube
import P4_Icosahedron
import P5_Dodecahedron
import P6_TruncatedTetrahedron
import P7_TruncatedOctahedron
import P8_TruncatedCube
import P9_TruncatedIcosahedron
import RenderHelper
import System.Exit

data State = State
  { polyhedraId :: IORef Int,
    cameraPos :: IORef (Int, Int, Double),
    lightingStatus :: IORef Capability,
    rotationMatrix :: IORef (M44 Float),
    isRotating :: IORef Capability
  }

constructState :: IO State
constructState = do
  polyhedra <- newIORef 0
  camera <- newIORef (90 :: Int, 270 :: Int, 8.0)
  lightingIs <- newIORef Enabled
  initMat <- newIORef (identity :: M44 Float)
  rotation <- newIORef Disabled
  return $
    State
      { polyhedraId = polyhedra,
        cameraPos = camera,
        lightingStatus = lightingIs,
        rotationMatrix = initMat,
        isRotating = rotation
      }

constructMenu :: State -> IO ()
constructMenu state =
  attachMenu
    RightButton
    ( Menu
        [ SubMenu
            "1. Tetrahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 0),
                  MenuEntry "Frame" (setPolyhedra state 1),
                  MenuEntry "Monochrome" (setPolyhedra state 2)
                ]
            ),
          SubMenu
            "2. Octahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 3),
                  MenuEntry "Frame" (setPolyhedra state 4),
                  MenuEntry "Monochrome" (setPolyhedra state 5)
                ]
            ),
          SubMenu
            "3. Cube"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 6),
                  MenuEntry "Frame" (setPolyhedra state 7),
                  MenuEntry "Monochrome" (setPolyhedra state 8)
                ]
            ),
          SubMenu
            "4. Icosahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 9),
                  MenuEntry "Frame" (setPolyhedra state 10),
                  MenuEntry "Monochrome" (setPolyhedra state 11)
                ]
            ),
          SubMenu
            "5. Dodecahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 12),
                  MenuEntry "Frame" (setPolyhedra state 13),
                  MenuEntry "Monochrome" (setPolyhedra state 14),
                  MenuEntry "Cutaway 1" (setPolyhedra state 15)
                ]
            ),
          SubMenu
            "6. Truncated Tetrahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 16),
                  MenuEntry "Frame" (setPolyhedra state 17),
                  MenuEntry "Monochrome" (setPolyhedra state 18),
                  MenuEntry "Cutaway 1" (setPolyhedra state 19)
                ]
            ),
          SubMenu
            "7. Truncated Octahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 20),
                  MenuEntry "Frame" (setPolyhedra state 21),
                  MenuEntry "Monochrome" (setPolyhedra state 22),
                  MenuEntry "Cutaway 1" (setPolyhedra state 23)
                ]
            ),
          SubMenu
            "8. Truncated Cube"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 24),
                  MenuEntry "Frame" (setPolyhedra state 25),
                  MenuEntry "Monochrome" (setPolyhedra state 26),
                  MenuEntry "Cutaway 1" (setPolyhedra state 27)
                ]
            ),
          SubMenu
            "9. Truncated Icosahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 28),
                  MenuEntry "Frame" (setPolyhedra state 29),
                  MenuEntry "Monochrome" (setPolyhedra state 30),
                  MenuEntry "Cutaway 1" (setPolyhedra state 31)
                ]
            ),
          SubMenu
            "10. Truncated Dodecahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 32),
                  MenuEntry "Frame" (setPolyhedra state 33),
                  MenuEntry "Monochrome" (setPolyhedra state 34),
                  MenuEntry "Cutaway 1" (setPolyhedra state 35)
                ]
            ),
          SubMenu
            "11. Cuboctahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 36),
                  MenuEntry "Frame" (setPolyhedra state 37),
                  MenuEntry "Monochrome" (setPolyhedra state 38),
                  MenuEntry "Cutaway 1" (setPolyhedra state 39)
                ]
            ),
          SubMenu
            "12. Icosidodecahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 40),
                  MenuEntry "Frame" (setPolyhedra state 41),
                  MenuEntry "Monochrome" (setPolyhedra state 42),
                  MenuEntry "Cutaway 1" (setPolyhedra state 43)
                ]
            ),
          SubMenu
            "13. Rhombicuboctahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 44),
                  MenuEntry "Frame" (setPolyhedra state 45),
                  MenuEntry "Monochrome" (setPolyhedra state 46)
                ]
            ),
          SubMenu
            "14. Rhombicosidodecahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 47),
                  MenuEntry "Frame" (setPolyhedra state 48),
                  MenuEntry "Monochrome" (setPolyhedra state 49)
                ]
            ),
          SubMenu
            "15. Truncated Cuboctahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 50),
                  MenuEntry "Frame" (setPolyhedra state 51),
                  MenuEntry "Monochrome" (setPolyhedra state 52)
                ]
            ),
          SubMenu
            "16. Truncated Icosidodecahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 53),
                  MenuEntry "Frame" (setPolyhedra state 54),
                  MenuEntry "Monochrome" (setPolyhedra state 55)
                ]
            ),
          SubMenu
            "17. Snub Cube"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 56),
                  MenuEntry "Frame" (setPolyhedra state 57),
                  MenuEntry "Monochrome" (setPolyhedra state 58)
                ]
            ),
          SubMenu
            "18. Snub Dodecahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 59),
                  MenuEntry "Frame" (setPolyhedra state 60),
                  MenuEntry "Monochrome" (setPolyhedra state 61)
                ]
            ),
          SubMenu
            "19. Stellated Octahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 62),
                  MenuEntry "Frame" (setPolyhedra state 63),
                  MenuEntry "Monochrome" (setPolyhedra state 64),
                  MenuEntry "Cutaway 1" (setPolyhedra state 65),
                  MenuEntry "Cutaway 2" (setPolyhedra state 66),
                  MenuEntry "Cutaway 3" (setPolyhedra state 67)
                ]
            ),
          SubMenu
            "20. Small Stellated Dodecahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 68),
                  MenuEntry "Frame" (setPolyhedra state 69),
                  MenuEntry "Monochrome" (setPolyhedra state 70),
                  MenuEntry "Cutaway 1" (setPolyhedra state 71)
                ]
            ),
          SubMenu
            "21. Great Dodecahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 72),
                  MenuEntry "Frame" (setPolyhedra state 73),
                  MenuEntry "Monochrome" (setPolyhedra state 74)
                ]
            ),
          SubMenu
            "22. Great Stellated Dodecahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 75),
                  MenuEntry "Frame" (setPolyhedra state 76),
                  MenuEntry "Monochrome" (setPolyhedra state 77),
                  MenuEntry "Cutaway 1" (setPolyhedra state 78)
                ]
            ),
          SubMenu
            "24. Great Icosahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 79),
                  MenuEntry "Frame" (setPolyhedra state 80),
                  MenuEntry "Monochrome" (setPolyhedra state 81)
                ]
            ),
          SubMenu
            "23. Cube and Octahedron Combination"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 82),
                  MenuEntry "Frame" (setPolyhedra state 83),
                  MenuEntry "Monochrome" (setPolyhedra state 84),
                  MenuEntry "Cutaway 1" (setPolyhedra state 85)
                ]
            ),
          MenuEntry "Exit" exitSuccess
        ]
    )

setPolyhedra :: State -> Int -> MenuCallback
setPolyhedra state polyhedra = do
  polyhedraId state $= polyhedra
  postRedisplay Nothing

toggleLighting :: State -> IO ()
toggleLighting state = do
  currentStatus <- readIORef (lightingStatus state)
  let newStatus = if currentStatus == Enabled then Disabled else Enabled
  lighting $= newStatus
  lightingStatus state $= newStatus
  postRedisplay Nothing

nextValue :: Int -> Int
nextValue polyhedra = if polyhedra == 85 then 0 else polyhedra + 1

showPolyhedra :: State -> DisplayCallback
showPolyhedra state = do
  polyhedra <- get $ polyhedraId state

  rotMatrix <- get $ rotationMatrix state
  let rotArray = concatMap Data.Foldable.toList (Data.Foldable.toList rotMatrix)
  openGLRotMatr <- newMatrix RowMajor rotArray :: IO (GLmatrix GLfloat)
  multMatrix openGLRotMatr

  case polyhedra of
    0 -> renderTetrahedron
    1 -> renderTetrahedronFrame green
    2 -> renderMonochromeTetrahedron
    3 -> renderOctahedron
    4 -> renderOctahedronFrame green
    5 -> renderMonochromeOctahedron
    6 -> renderCube
    7 -> renderCubeFrame green
    8 -> renderMonochromeCube
    9 -> renderIcosahedron
    10 -> renderIcosahedronFrame green
    11 -> renderMonochromeIcosahedron
    12 -> renderDodecahedron
    13 -> renderDodecahedronFrame green
    14 -> renderMonochromeDodecahedron
    15 -> renderDodecahedronCutaway_1
    16 -> renderTruncatedTetrahedron
    17 -> renderTruncatedTetrahedronFrame green
    18 -> renderMonochromeTruncatedTetrahedron
    19 -> renderTruncatedTetrahedronCutaway_1
    20 -> renderTruncatedOctahedron
    21 -> renderTruncatedOctahedronFrame green
    22 -> renderMonochromeTruncatedOctahedron
    23 -> renderTruncatedOctahedronCutaway_1
    24 -> renderTruncatedCube
    25 -> renderTruncatedCubeFrame green
    26 -> renderMonochromeTruncatedCube
    27 -> renderTruncatedCubeCutaway_1
    28 -> renderTruncatedIcosahedron
    29 -> renderTruncatedIcosahedronFrame green
    30 -> renderMonochromeTruncatedIcosahedron
    31 -> renderTruncatedIcosahedronCutaway_1
    32 -> renderTruncatedDodecahedron
    33 -> renderTruncatedDodecahedronFrame green
    34 -> renderMonochromeTruncatedDodecahedron
    35 -> renderTruncatedDodecahedronCutaway_1
    36 -> renderCuboctahedron
    37 -> renderCuboctahedronFrame green
    38 -> renderMonochromeCuboctahedron
    39 -> renderCuboctahedronCutaway_1
    40 -> renderIcosidodecahedron
    41 -> renderIcosidodecahedronFrame green
    42 -> renderMonochromeIcosidodecahedron
    43 -> renderIcosidodecahedronCutaway_1
    44 -> renderRhombicuboctahedron
    45 -> renderRhombicuboctahedronFrame green
    46 -> renderMonochromeRhombicuboctahedron
    47 -> renderRhombicosidodecahedron
    48 -> renderRhombicosidodecahedronFrame green
    49 -> renderMonochromeRhombicosidodecahedron
    50 -> renderTruncatedCuboctahedron
    51 -> renderTruncatedCuboctahedronFrame green
    52 -> renderMonochromeTruncatedCuboctahedron
    53 -> renderTruncatedIcosidodecahedron
    54 -> renderTruncatedIcosidodecahedronFrame green
    55 -> renderMonochromeTruncatedIcosidodecahedron
    56 -> renderSnubCube
    57 -> renderSnubCubeFrame green
    58 -> renderMonochromeSnubCube
    59 -> renderSnubDodecahedron
    60 -> renderSnubDodecahedronFrame green
    61 -> renderMonochromeSnubDodecahedron
    62 -> renderStellatedOctahedron
    63 -> renderStellatedOctahedronFrame green
    64 -> renderMonochromeStellatedOctahedron
    65 -> renderStellatedOctahedronCutaway_1
    66 -> renderStellatedOctahedronCutaway_2
    67 -> renderStellatedOctahedronCutaway_3
    68 -> renderSmallStellatedDodecahedron
    69 -> renderSmallStellatedDodecahedronFrame green
    70 -> renderMonochromeSmallStellatedDodecahedron
    71 -> renderSmallStellatedDodecahedronCutaway_1
    72 -> renderGreatDodecahedron
    73 -> renderGreatDodecahedronFrame green
    74 -> renderMonochromeGreatDodecahedron
    75 -> renderGreatStellatedDodecahedron
    76 -> renderGreatStellatedDodecahedronFrame green
    77 -> renderMonochromeGreatStellatedDodecahedron
    78 -> renderGreatStellatedDodecahedronFrameCutaway_1
    79 -> renderGreatIcosahedron
    80 -> renderGreatIcosahedronFrame green
    81 -> renderMonochromeGreatIcosahedron
    82 -> renderCubeAndOctahedron
    83 -> renderCubeAndOctahedronFrame
    84 -> renderCubeAndOctahedronMonochrome
    85 -> renderCubeAndOctahedronCutaway_1
  postRedisplay Nothing

myMotionCallback :: State -> Position -> MotionCallback
myMotionCallback state (Position startX startY) (Position currentX currentY)
  | startX /= currentX && startY /= currentY = do
    let fstartX = fromIntegral startX :: Float
    let fstartY = fromIntegral startY :: Float
    let fcurrentX = fromIntegral currentX :: Float
    let fcurrentY = fromIntegral currentY :: Float
    (Size width height) <- get screenSize
    let windowWidth = fromIntegral width :: Float
    let windowHeight = fromIntegral height :: Float

    let startPoint = projectOntoSurface (windowWidth, windowHeight) (Vector3 fstartX fstartY 0.0)
    let endPoint = projectOntoSurface (windowWidth, windowHeight) (Vector3 fcurrentX fcurrentY 0.0)

    let axis@(Vector3 axisX axisY axisZ) = normalizeVector $ vectorCrossProduct startPoint endPoint
    let angle = acos (vectorDotProduct startPoint endPoint)

    let axisV3 = Linear.V3.V3 axisX axisY axisZ
    let rotationQ = Linear.Quaternion.axisAngle axisV3 (angle * 2)
    let newRotMatrix = Linear.Matrix.mkTransformation rotationQ (V3 0 0 0)
    currentRotMatrix <- get $ rotationMatrix state

    rotationMatrix state $= newRotMatrix
  | otherwise = return ()

myKeyboardCallback :: State -> KeyboardMouseCallback
myKeyboardCallback state (MouseButton LeftButton) Down _ pos = do
  rotating <- get $ isRotating state
  if rotating == Disabled
    then
      ( do
          cameraPos state $= (90 :: Int, 270 :: Int, 8.0)
          passiveMotionCallback $= Just (myMotionCallback state pos)
          isRotating state $= Enabled
      )
    else
      ( do
          rotationMatrix state $= (identity :: M44 Float)
          passiveMotionCallback $= Nothing
          isRotating state $= Disabled
      )
myKeyboardCallback state (Char '\32') Down _ _ = do
  polyhedraId state $~ nextValue
  postRedisplay Nothing
myKeyboardCallback state (Char '\50') Down _ _ = do
  rotationMatrix state $= (identity :: M44 Float)
  cameraPos state $= (90 :: Int, 270 :: Int, 8.0)
  passiveMotionCallback $= Nothing
  isRotating state $= Disabled
  postRedisplay Nothing
myKeyboardCallback _ (Char '\27') Down _ _ = exitSuccess
myKeyboardCallback state (Char '\49') Down _ _ = toggleLighting state
myKeyboardCallback State {cameraPos = pPos} key _ _ _ = keyForPos pPos key

display :: State -> DisplayCallback
display state = do
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  setPointOfView $ cameraPos state
  preservingMatrix $ showPolyhedra state
  swapBuffers

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, RGBMode, DoubleBuffered]
  initialWindowSize $= Size 800 600
  initialWindowPosition $= Position 0 0
  _ <- createWindow "OpenGL Polyhedrons"
  state <- constructState

  shadeModel $= Smooth
  rescaleNormal $= Enabled

  materialSpecular Front $= Color4 0.5 0.5 0.5 1
  materialShininess Front $= 128
  colorMaterial $= Just (Front, AmbientAndDiffuse)

  position (Light 0) $= Vertex4 0 0 2 0

  lightModelAmbient $= Color4 0.1 0.1 0.1 1

  status <- readIORef (lightingStatus state)
  lighting $= status
  light (Light 0) $= Enabled

  depthFunc $= Just Less

  clearColor $= Color4 0.0 0.0 0.0 1.0

  constructMenu state
  displayCallback $= display state
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (myKeyboardCallback state)

  mainLoop