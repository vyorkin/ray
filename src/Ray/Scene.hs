module Ray.Scene
  ( Scene(..)

  , render
  , project

  , example

  , module Ray.Scene.Types
  ) where

import Foreign.C.Types (CInt, CFloat)
import SDL (V2(..), V3(..))

import Ray.Buffer (Buffer(..))
import qualified Ray.Buffer as Buffer
import qualified Ray.Color as Color
import Ray.Scene.Types (Scene(..), Camera(..), Sphere(..), mkSphere)

render :: Scene -> Buffer -> Buffer
render Scene{..} buf = buf

project :: V2 CInt -> Scene -> V2 CFloat -> V3 CFloat
project (V2 cw ch) Scene{viewportSize = V2 vw vh, ..} (V2 x y) =
  let dw = vw / fromIntegral cw
      dh = vh / fromIntegral ch
   in V3 (x * dw) (y * dh) projPlaneZ

example :: Scene
example =
  let viewportSize = V2 1.0 1.0
      projPlaneZ = 1.0
      spheres = [s1, s2, s3]
      camera = Camera (V3 0.0 0.0 0.0)
   in Scene{..}
  where
    s1 = mkSphere (V3 0 (-1) 3) Color.red
    s2 = mkSphere (V3 2 0 4) Color.blue
    s3 = mkSphere (V3 (-2) 0 4) Color.green
