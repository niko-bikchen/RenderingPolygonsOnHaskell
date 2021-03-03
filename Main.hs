import Data.Foldable
import Data.IORef
import Graphics.UI.GLUT
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
    lightingStatus :: IORef Capability
  }

constructState :: IO State
constructState = do
  polyhedra <- newIORef 0
  camera <- newIORef (90 :: Int, 270 :: Int, 8.0)
  lightingIs <- newIORef Enabled
  return $
    State
      { polyhedraId = polyhedra,
        cameraPos = camera,
        lightingStatus = lightingIs
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
                  MenuEntry "Monochrome" (setPolyhedra state 14)
                ]
            ),
          SubMenu
            "6. Truncated Tetrahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 15),
                  MenuEntry "Frame" (setPolyhedra state 16),
                  MenuEntry "Monochrome" (setPolyhedra state 17)
                ]
            ),
          SubMenu
            "7. Truncated Octahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 18),
                  MenuEntry "Frame" (setPolyhedra state 19),
                  MenuEntry "Monochrome" (setPolyhedra state 20)
                ]
            ),
          SubMenu
            "8. Truncated Cube"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 21),
                  MenuEntry "Frame" (setPolyhedra state 22),
                  MenuEntry "Monochrome" (setPolyhedra state 23)
                ]
            ),
          SubMenu
            "9. Truncated Icosahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 24),
                  MenuEntry "Frame" (setPolyhedra state 25),
                  MenuEntry "Monochrome" (setPolyhedra state 26)
                ]
            ),
          SubMenu
            "10. Truncated Dodecahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 27),
                  MenuEntry "Frame" (setPolyhedra state 28),
                  MenuEntry "Monochrome" (setPolyhedra state 29)
                ]
            ),
          SubMenu
            "11. Cuboctahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 30),
                  MenuEntry "Frame" (setPolyhedra state 31),
                  MenuEntry "Monochrome" (setPolyhedra state 32)
                ]
            ),
          SubMenu
            "12. Icosidodecahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 33),
                  MenuEntry "Frame" (setPolyhedra state 34),
                  MenuEntry "Monochrome" (setPolyhedra state 35)
                ]
            ),
          SubMenu
            "13. Rhombicuboctahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 36),
                  MenuEntry "Frame" (setPolyhedra state 37),
                  MenuEntry "Monochrome" (setPolyhedra state 38)
                ]
            ),
          SubMenu
            "14. Rhombicosidodecahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 39),
                  MenuEntry "Frame" (setPolyhedra state 40),
                  MenuEntry "Monochrome" (setPolyhedra state 41)
                ]
            ),
          SubMenu
            "15. Truncated cuboctahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 42),
                  MenuEntry "Frame" (setPolyhedra state 43),
                  MenuEntry "Monochrome" (setPolyhedra state 44)
                ]
            ),
          SubMenu
            "16. Truncated Icosidodecahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 45),
                  MenuEntry "Frame" (setPolyhedra state 46),
                  MenuEntry "Monochrome" (setPolyhedra state 47)
                ]
            ),
          SubMenu
            "17. Snub Cube"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 48),
                  MenuEntry "Frame" (setPolyhedra state 49),
                  MenuEntry "Monochrome" (setPolyhedra state 50)
                ]
            ),
          SubMenu
            "18. Snub Dodecahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 51),
                  MenuEntry "Frame" (setPolyhedra state 52),
                  MenuEntry "Monochrome" (setPolyhedra state 53)
                ]
            ),
          SubMenu
            "19. Stellated Octahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 54),
                  MenuEntry "Frame" (setPolyhedra state 55),
                  MenuEntry "Monochrome" (setPolyhedra state 56),
                  MenuEntry "Cutaway 1" (setPolyhedra state 57),
                  MenuEntry "Cutaway 2" (setPolyhedra state 58),
                  MenuEntry "Cutaway 3" (setPolyhedra state 59)
                ]
            ),
          SubMenu
            "20. Small Stellated Dodecahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 60),
                  MenuEntry "Frame" (setPolyhedra state 61),
                  MenuEntry "Monochrome" (setPolyhedra state 62),
                  MenuEntry "Cutaway 1" (setPolyhedra state 63)
                ]
            ),
          SubMenu
            "21. Great Dodecahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 64),
                  MenuEntry "Frame" (setPolyhedra state 65),
                  MenuEntry "Monochrome" (setPolyhedra state 66)
                ]
            ),
          SubMenu
            "22. Great Stellated Dodecahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 67),
                  MenuEntry "Frame" (setPolyhedra state 68),
                  MenuEntry "Monochrome" (setPolyhedra state 69),
                  MenuEntry "Cutaway 1" (setPolyhedra state 70)
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
nextValue polyhedra = if polyhedra == 70 then 0 else polyhedra + 1

showPolyhedra :: State -> DisplayCallback
showPolyhedra state = do
  polyhedra <- get $ polyhedraId state
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
    15 -> renderTruncatedTetrahedron
    16 -> renderTruncatedTetrahedronFrame green
    17 -> renderMonochromeTruncatedTetrahedron
    18 -> renderTruncatedOctahedron
    19 -> renderTruncatedOctahedronFrame green
    20 -> renderMonochromeTruncatedOctahedron
    21 -> renderTruncatedCube
    22 -> renderTruncatedCubeFrame green
    23 -> renderMonochromeTruncatedCube
    24 -> renderTruncatedIcosahedron
    25 -> renderTruncatedIcosahedronFrame green
    26 -> renderMonochromeTruncatedIcosahedron
    27 -> renderTruncatedDodecahedron
    28 -> renderTruncatedDodecahedronFrame green
    29 -> renderMonochromeTruncatedDodecahedron
    30 -> renderCuboctahedron
    31 -> renderCuboctahedronFrame green
    32 -> renderMonochromeCuboctahedron
    33 -> renderIcosidodecahedron
    34 -> renderIcosidodecahedronFrame green
    35 -> renderMonochromeIcosidodecahedron
    36 -> renderRhombicuboctahedron
    37 -> renderRhombicuboctahedronFrame green
    38 -> renderMonochromeRhombicuboctahedron
    39 -> renderRhombicosidodecahedron
    40 -> renderRhombicosidodecahedronFrame green
    41 -> renderMonochromeRhombicosidodecahedron
    42 -> renderTruncatedCuboctahedron
    43 -> renderTruncatedCuboctahedronFrame green
    44 -> renderMonochromeTruncatedCuboctahedron
    45 -> renderTruncatedIcosidodecahedron
    46 -> renderTruncatedIcosidodecahedronFrame green
    47 -> renderMonochromeTruncatedIcosidodecahedron
    48 -> renderSnubCube
    49 -> renderSnubCubeFrame green
    50 -> renderMonochromeSnubCube
    51 -> renderSnubDodecahedron
    52 -> renderSnubDodecahedronFrame green
    53 -> renderMonochromeSnubDodecahedron
    54 -> renderStellatedOctahedron
    55 -> renderStellatedOctahedronFrame green
    56 -> renderMonochromeStellatedOctahedron
    57 -> renderStellatedOctahedronCutaway_1
    58 -> renderStellatedOctahedronCutaway_2
    59 -> renderStellatedOctahedronCutaway_3
    60 -> renderSmallStellatedDodecahedron
    61 -> renderSmallStellatedDodecahedronFrame green
    62 -> renderMonochromeSmallStellatedDodecahedron
    63 -> renderSmallStellatedDodecahedronCutaway_1
    64 -> renderGreatDodecahedron
    65 -> renderGreatDodecahedronFrame green
    66 -> renderMonochromeGreatDodecahedron
    67 -> renderGreatStellatedDodecahedron
    68 -> renderGreatStellatedDodecahedronFrame green
    69 -> renderMonochromeGreatStellatedDodecahedron
    70 -> renderGreatStellatedDodecahedronFrameCutaway_1

myKeyboardCallback :: State -> KeyboardMouseCallback
myKeyboardCallback state (Char '\32') Down _ _ = do
  polyhedraId state $~ nextValue
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
  (progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, RGBMode, DoubleBuffered]
  initialWindowSize $= Size 500 500
  initialWindowPosition $= Position 0 0
  _ <- createWindow progName
  state <- constructState

  shadeModel $= Smooth
  rescaleNormal $= Enabled

  materialSpecular Front $= Color4 0.5 0.5 0.5 1
  materialShininess Front $= 128
  colorMaterial $= Just (Front, Diffuse)

  position (Light 0) $= Vertex4 0 0 2 0

  status <- readIORef (lightingStatus state)
  lighting $= status
  light (Light 0) $= Enabled

  depthFunc $= Just Less

  clearColor $= Color4 0 0 0 0

  constructMenu state
  displayCallback $= display state
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (myKeyboardCallback state)

  mainLoop