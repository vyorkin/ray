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

update :: Texture -> CInt -> Buffer -> IO Texture
update (Texture tex size) pitch buf = do
  -- Raw pixels to draw onto the texture
  let pixels = Buffer.toByteString buf
  tex' <- SDL.updateTexture tex Nothing pixels pitch
  pure $ Texture tex' size

render :: Texture -> SDL.Renderer -> IO ()
render (Texture tex _) renderer = do
  SDL.clear renderer
  SDL.copy renderer tex Nothing Nothing
  SDL.present renderer

destroy :: Texture -> IO ()
destroy = SDL.destroyTexture . unwrap
