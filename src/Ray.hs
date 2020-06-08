{-# LANGUAGE BangPatterns #-}
module Ray
  ( run
  ) where

import Foreign.C.Types (CInt)
import Control.Monad (when, unless)
import Data.Word (Word32)

import SDL (V2(..), ($=))
import qualified SDL

import Ray.Config (Config(..))
import qualified Ray.Events as Events
import Ray.Scene (Scene(..))
import qualified Ray.Scene as Scene
import Ray.Canvas (Canvas(..), newCanvas)
import qualified Ray.Canvas as Canvas

data Context = Context
  { window :: !SDL.Window
  , renderer :: !SDL.Renderer
  , canvas :: !Canvas
  , scene :: !Scene
  }

run :: Config -> IO ()
run config = do
  ctx <- setup config
  ticks <- SDL.ticks
  loop ticks 0 ctx
  cleanUp ctx

setup :: Config -> IO Context
setup Config{..} = do
  SDL.initialize [SDL.InitVideo]
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  renderQuality <- SDL.get SDL.HintRenderScaleQuality
  when (renderQuality /= SDL.ScaleLinear) $
    putStrLn "Warning: Linear texture filtering not enabled!"
  window <- createWindow windowSize
  SDL.showWindow window
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  let scene = Scene.example
  canvas <- newCanvas renderer windowSize
  pure $ Context{..}
  where
    windowSize = fromIntegral <$> V2 width height

loop :: Word32 -> Word32 -> Context -> IO ()
loop !lastTick !nframes ctx = do
  quit <- Events.handle
  unless quit $
    if nframes < 10
      then draw ctx >>= loop lastTick (nframes + 1)
      else do
        ticks <- SDL.ticks
        let diff = fromIntegral $ ticks - lastTick
            fps  = fromIntegral nframes / diff * 1000
        putStrLn $ "FPS: " <> show (fps :: Double)
        draw ctx >>= loop ticks 0

draw :: Context -> IO Context
draw Context{..} = do
  let cvs = Scene.render scene canvas
  -- let cvs = Scene.renderTest canvas
  cvs' <- Canvas.update cvs renderer
  pure Context { canvas = cvs', ..}

cleanUp :: Context -> IO ()
cleanUp Context{..} = do
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

createWindow :: V2 CInt -> IO SDL.Window
createWindow size =
  SDL.createWindow "Ray tracer" SDL.defaultWindow
    { SDL.windowInitialSize = size
    , SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL { SDL.glProfile = SDL.Core SDL.Normal 4 0 }
    }
