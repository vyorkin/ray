module Ray.Texture
  ( Texture(..)
  , unwrap
  , new
  , update
  , render
  , destroy
  ) where

import Foreign.C.Types (CInt)
import qualified SDL
import SDL (V2(..))

import Ray.Buffer (Buffer)
import qualified Ray.Buffer as Buffer

data Texture = Texture SDL.Texture (V2 CInt)

unwrap :: Texture -> SDL.Texture
unwrap (Texture tex _) = tex

new :: SDL.Renderer -> V2 CInt -> SDL.TextureAccess -> IO Texture
new renderer size access = Texture
  <$> SDL.createTexture renderer SDL.RGBA8888 access size
  <*> pure size

update :: Texture -> Buffer -> IO Texture
update (Texture tex size@(V2 w _)) buf = do
  let -- Raw pixels to draw onto the texture
      pixels = Buffer.toByteString buf
      -- Size in bytes of a single row of pixels
      pitch = w * bytesInWord32
  tex' <- SDL.updateTexture tex Nothing pixels pitch
  pure $ Texture tex' size
  where
    bytesInWord32 :: CInt
    bytesInWord32 = 4

render :: Texture -> SDL.Renderer -> IO ()
render (Texture tex _) renderer = do
  SDL.clear renderer
  SDL.copy renderer tex Nothing Nothing
  SDL.present renderer

destroy :: Texture -> IO ()
destroy = SDL.destroyTexture . unwrap
