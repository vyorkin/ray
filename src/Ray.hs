module Ray (run) where

import Control.Monad (when, unless)

import SDL (V2(..), ($=))
import qualified SDL

import Ray.Config (Config(..))
import qualified Ray.Events as Events
import Ray.Texture (Texture)
import qualified Ray.Texture as Texture
import Ray.Buffer (Buffer, mkBuffer)
import qualified Ray.Buffer as Buffer
import qualified Ray.Color as Color

data Context = Context
  { window :: !SDL.Window
  , renderer :: !SDL.Renderer
  , texture :: !Texture
  , buffer :: !Buffer
  }

run :: Config -> IO ()
run config = do
  ctx <- setup config
  loop ctx
  cleanUp ctx

setup :: Config -> IO Context
setup Config{..} = do
  SDL.initialize [SDL.InitVideo]
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  renderQuality <- SDL.get SDL.HintRenderScaleQuality
  when (renderQuality /= SDL.ScaleLinear) $
    putStrLn "Warning: Linear texture filtering not enabled!"

  let windowSize = V2 (fromIntegral width) (fromIntegral height)
  window <- SDL.createWindow "Ray tracer" SDL.defaultWindow
    { SDL.windowInitialSize = windowSize
    , SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
    }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  texture <- Texture.new renderer windowSize SDL.TextureAccessStatic
  let
    putColor = Buffer.write windowSize
    buffer = putColor (V2 10 10) Color.red
           $ putColor (V2 100 100) Color.red
           $ mkBuffer windowSize Color.black
  pure Context{..}

loop :: Context -> IO ()
loop Context{..} = do
  quit <- Events.handle
  unless quit do
    tex <- Texture.update texture buffer
    Texture.render texture renderer
    loop $ Context{ texture = tex, .. }

cleanUp :: Context -> IO ()
cleanUp Context{..} = do
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
