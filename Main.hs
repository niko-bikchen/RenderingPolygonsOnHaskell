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
import P1_Tetrahedron
import P2_Octahedron
import P3_Cube
import P4_Icosahedron
import P5_Dodecahedron
import P6_TruncatedTetrahedron
import P7_TruncatedOctahedron
import P8_TruncatedCube
import P9_TruncatedIcosahedron
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
  return $ State {polyhedraId = polyhedra, cameraPos = camera, lightingStatus = lightingIs}

constructMenu :: State -> IO ()
constructMenu state =
  attachMenu
    RightButton
    ( Menu
        [ SubMenu
            "1. Tetrahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 0),
                  MenuEntry "Frame" (setPolyhedra state 18),
                  MenuEntry "Monochrome" (setPolyhedra state 19)
                ]
            ),
          SubMenu
            "2. Octahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 1),
                  MenuEntry "Frame" (setPolyhedra state 20),
                  MenuEntry "Monochrome" (setPolyhedra state 21)
                ]
            ),
          SubMenu
            "3. Cube"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 2),
                  MenuEntry "Frame" (setPolyhedra state 22),
                  MenuEntry "Monochrome" (setPolyhedra state 23)
                ]
            ),
          SubMenu
            "4. Icosahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 3),
                  MenuEntry "Frame" (setPolyhedra state 24),
                  MenuEntry "Monochrome" (setPolyhedra state 25)
                ]
            ),
          SubMenu
            "5. Dodecahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 4),
                  MenuEntry "Frame" (setPolyhedra state 26),
                  MenuEntry "Monochrome" (setPolyhedra state 27)
                ]
            ),
          SubMenu
            "6. Truncated Tetrahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 5),
                  MenuEntry "Frame" (setPolyhedra state 28),
                  MenuEntry "Monochrome" (setPolyhedra state 29)
                ]
            ),
          SubMenu
            "7. Truncated Octahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 6)
                ]
            ),
          SubMenu
            "8. Truncated Cube"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 7)
                ]
            ),
          SubMenu
            "9. Truncated Icosahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 8)
                ]
            ),
          SubMenu
            "10. Truncated Dodecahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 9)
                ]
            ),
          SubMenu
            "11. Cuboctahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 10)
                ]
            ),
          SubMenu
            "12. Icosidodecahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 11)
                ]
            ),
          SubMenu
            "13. Rhombicuboctahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 12)
                ]
            ),
          SubMenu
            "14. Rhombicosidodecahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 13)
                ]
            ),
          SubMenu
            "15. Truncated cuboctahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 14)
                ]
            ),
          SubMenu
            "16. Truncated Icosidodecahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 15)
                ]
            ),
          SubMenu
            "17. Snub Cube"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 16)
                ]
            ),
          SubMenu
            "18. Snub Dodecahedron"
            ( Menu
                [ MenuEntry "Colored" (setPolyhedra state 17)
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
nextValue polyhedra = if polyhedra == 27 then 0 else polyhedra + 1

showPolyhedra :: Int -> DisplayCallback
showPolyhedra polyhedra = do
  case polyhedra of
    0 -> renderTetrahedron
    1 -> renderOctahedron
    2 -> renderCube
    3 -> renderIcosahedron
    4 -> renderDodecahedron
    5 -> renderTruncatedTetrahedron
    6 -> renderTruncatedOctahedron
    7 -> renderTruncatedCube
    8 -> renderTruncatedIcosahedron
    9 -> renderTruncatedDodecahedron
    10 -> renderCuboctahedron
    11 -> renderIcosidodecahedron
    12 -> renderRhombicuboctahedron
    13 -> renderRhombicosidodecahedron
    14 -> renderTruncatedCuboctahedron
    15 -> renderTruncatedIcosidodecahedron
    16 -> renderSnubCube
    17 -> renderSnubDodecahedron
    18 -> renderTetrahedronFrame
    19 -> renderMonochromeTetrahedron
    20 -> renderOctahedronFrame
    21 -> renderMonochromeOctahedron
    22 -> renderCubeFrame
    23 -> renderMonochromeCube
    24 -> renderIcosahedronFrame
    25 -> renderMonochromeIcosahedron
    26 -> renderDodecahedronFrame
    27 -> renderMonochromeDodecahedron
    28 -> renderTruncatedTetrahedronFrame
    29 -> renderMonochromeTruncatedTetrahedron

myKeyboardCallback :: State -> KeyboardMouseCallback
myKeyboardCallback state (MouseButton _) Down _ _ = do
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
  polyhedra <- get $ polyhedraId state
  preservingMatrix $ showPolyhedra polyhedra
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

  materialSpecular Front $= Color4 0.5 0.5 0.5 1
  materialShininess Front $= 128
  colorMaterial $= Just (Front, Diffuse)

  position (Light 0) $= Vertex4 0 0 2 0

  status <- readIORef (lightingStatus state)
  lighting $= status
  light (Light 0) $= Enabled

  depthFunc $= Just Less

  clearColor $= Color4 0 0 0 0
  cullFace $= Just Back

  constructMenu state
  displayCallback $= display state
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (myKeyboardCallback state)

  mainLoop