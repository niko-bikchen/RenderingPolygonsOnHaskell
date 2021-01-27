import Data.IORef
import Graphics.UI.GLUT
import OrbitPointOfView
import P10_TruncatedDodecahedron
import P11_Cuboctahedron
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
    cameraPos :: IORef (Int, Int, Double)
  }

constructState :: IO State
constructState = do
  polyhedra <- newIORef 0
  camera <- newIORef (90 :: Int, 270 :: Int, 8.0)
  return $ State {polyhedraId = polyhedra, cameraPos = camera}

constructMenu :: State -> IO ()
constructMenu state =
  attachMenu
    RightButton
    ( Menu
        [ SubMenu
            "1. Tetrahedron"
            ( Menu
                [ MenuEntry "Colored" (setState state 0)
                ]
            ),
          SubMenu
            "2. Octahedron"
            ( Menu
                [ MenuEntry "Colored" (setState state 1)
                ]
            ),
          SubMenu
            "3. Cube"
            ( Menu
                [ MenuEntry "Colored" (setState state 2)
                ]
            ),
          SubMenu
            "4. Icosahedron"
            ( Menu
                [ MenuEntry "Colored" (setState state 3)
                ]
            ),
          SubMenu
            "5. Dodecahedron"
            ( Menu
                [ MenuEntry "Colored" (setState state 4)
                ]
            ),
          SubMenu
            "6. Truncated Tetrahedron"
            ( Menu
                [ MenuEntry "Colored" (setState state 5)
                ]
            ),
          SubMenu
            "7. Truncated Octahedron"
            ( Menu
                [ MenuEntry "Colored" (setState state 6)
                ]
            ),
          SubMenu
            "8. Truncated Cube"
            ( Menu
                [ MenuEntry "Colored" (setState state 7)
                ]
            ),
          SubMenu
            "9. Truncated Icosahedron"
            ( Menu
                [ MenuEntry "Colored" (setState state 8)
                ]
            ),
          SubMenu
            "10. Truncated Dodecahedron"
            ( Menu
                [ MenuEntry "Colored" (setState state 9)
                ]
            ),
          SubMenu
            "11. Cuboctahedron"
            ( Menu
                [ MenuEntry "Colored" (setState state 10)
                ]
            ),
          MenuEntry "Exit" exitSuccess
        ]
    )

setState :: State -> Int -> MenuCallback
setState state polyhedra = do
  polyhedraId state $= polyhedra
  postRedisplay Nothing

nextValue :: Int -> Int
nextValue polyhedra = if polyhedra == 10 then 0 else polyhedra + 1

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

myKeyboardCallback :: State -> KeyboardMouseCallback
myKeyboardCallback state (MouseButton _) Down _ _ = do
  polyhedraId state $~ nextValue
  postRedisplay Nothing
myKeyboardCallback _ (Char '\27') Down _ _ = exitSuccess
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

  clearColor $= Color4 0 0 0 0
  cullFace $= Just Back

  constructMenu state
  displayCallback $= display state
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (myKeyboardCallback state)

  mainLoop