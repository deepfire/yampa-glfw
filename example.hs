{-# LANGUAGE Arrows, PackageImports #-}
import Control.Arrow
import Data.Monoid
import Data.VectorSpace

import "GLFW-b" Graphics.UI.GLFW
import Graphics.Rendering.OpenGL
import FRP.Yampa.GLFW.Adapter
import FRP.Yampa (SF, integral, delay, initially, edge, accumHoldBy, time)
import FRP.Yampa.Event

import GHC.Float

import Text.Printf

import Unsafe.Coerce
import System.IO.Unsafe
import Data.IORef

checkError :: String -> IO ()
checkError functionName = get errors >>= mapM_ reportError
   where reportError e =
            putStrLn (showError e ++ " detected in " ++ functionName)
         showError (Error category message) =
            "GL error " ++ show category ++ " (" ++ message ++ ")"

main = do
    simpleInit "Simple"
    cullFace $= Just Back
    frontFace $= CW
    shadeModel $= Smooth

    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

    lighting $= Enabled
    ambient (Light 0) $= Color4 0.1 0.1 0.1 (1::GLfloat)
    diffuse (Light 0) $= Color4 0.9 0.9 0.9 (1::GLfloat)
    position (Light 0) $= Vertex4 0.5 0.5 (-10) 0
    light (Light 0) $= Enabled
    colorMaterial $= Just (Front, AmbientAndDiffuse)

    checkError "init"

    adapt simple

simple :: Reaction
simple = proc ev -> do
    pos <- positionSim -< ev
    disp <- redraw -< ev
    reshapedAction <- fmap (actionIO . reshape) ^<< resized -< ev
    returnA -< mconcat [
      reshapedAction,
      disp `tag` actionIO (display pos)
      ]

mkVertex :: Float -> Float -> Float -> IO ()
mkVertex x y z =
    vertex $ (Vertex3 :: GLfloat -> GLfloat -> GLfloat -> Vertex3 GLfloat) (realToFrac x) (realToFrac y) (realToFrac z)

display (x, y) = do
    clear [ ColorBuffer, DepthBuffer ]

    preservingMatrix $ do
        translate (Vector3 (realToFrac x) (realToFrac y) (0 :: GLfloat))
        color $ Color4 0.2 0.8 0 (0.5::GLfloat)
        renderPrimitive Triangles $ do
            mkVertex  (-0.1) (-0.1) (0)
            mkVertex  (-0.1) ( 0.2) (0)
            mkVertex  ( 0.2) (-0.1) (0)

    swapBuffers
reshape sz@(Size w h) = do
    let b = fromIntegral (w `min` h) * 2
        w' = fromIntegral w / b
        h' = fromIntegral h / b

    viewport $= (Position 0 0, sz)

    matrixMode $= Projection
    loadIdentity
    frustum (-w') w' (-h') h' 2 100

    matrixMode $= Modelview 0
    loadIdentity

    translate (Vector3 0 0 (-4 :: GLfloat))

pr :: String -> b -> b
pr x exp = seq (unsafePerformIO $ putStrLn x) exp

oldRef = unsafePerformIO $ newIORef (0 :: Int, 0.0, (0, 0))

v2len :: (Float, Float) -> Float
v2len (x, y) = sqrt(x*x + y*y)

watcher :: Double -> (Float, Float) -> IO ()
watcher t pos = do
  (oldi, oldt, oldp) <- readIORef oldRef
  writeIORef oldRef (oldi + 1, t, pos)
  let dt = t - oldt
      dp = v2len $ pos ^-^ oldp
  printf "; %d t=%3.3f dt=%3.3f dp=%3.3f es=%3.3f\n" oldi t dt dp (dp / (double2Float dt))
  return ()

positionSim :: SF (Event UI) (Float, Float)
positionSim = proc ev -> do
               d <- moveDirection -< ev
               (Vector2 tx ty) <- simpleMousePosition -< ev
               t <- time -< ev
               rec
                   let mpos = (tx, ty)
                       dpos = mpos ^-^ pos
                       speed = normalized dpos ^* 0.5 ^* d
                   pos <- integral <<< delay 0.1 zeroV -< speed
               returnA -< -- seq (unsafePerformIO $ watcher t pos)
                                 pos
moveDirection :: SF (Event UI) Float
moveDirection = mouseButtonPressed MouseButton0 >>> edge >>> accumHoldBy (const . negate) 1
