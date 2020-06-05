module Ray.Canvas
  ( Canvas(..)
  , newCanvas
  , putPixel
  , update
  ) where

import Foreign.C.Types (CInt)
import SDL (V2(..))
import qualified SDL

import Ray.Texture (Texture)
import qualified Ray.Texture as Texture
import Ray.Buffer (Buffer, mkBuffer)
import qualified Ray.Buffer as Buffer
import Ray.Color (Color)
import qualified Ray.Color as Color

data Canvas = Canvas
  { size :: !(V2 CInt)
  -- ^ Canvas size
  , pitch :: !CInt
  -- ^ Size in bytes of a single row of pixels
  , texture :: !Texture
  , buffer :: !Buffer
  }

newCanvas :: SDL.Renderer -> V2 CInt -> IO Canvas
newCanvas renderer size@(V2 w _) = do
  texture <- Texture.new renderer size SDL.TextureAccessStatic
  let buffer = mkBuffer size Color.black
      pitch = w * colorBytes
  pure Canvas{..}
  where
    colorBytes :: CInt
    colorBytes = 4

putPixel :: V2 CInt -> Color -> Canvas -> Canvas
putPixel point color canvas =
  canvas { buffer = Buffer.write (size canvas) point color (buffer canvas) }

update :: Canvas -> SDL.Renderer -> IO Canvas
update Canvas{..} renderer = do
   tex <- Texture.update texture pitch buffer
   Texture.render texture renderer
   pure $ Canvas{ texture = tex, .. }
