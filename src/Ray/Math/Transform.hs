module Ray.Math.Transform
  ( project
  ) where

import Foreign.C.Types (CInt, CFloat)
import SDL (V2(..), V3(..))

-- | Projects a point on canvas to viewport.
project
  :: V2 CInt   -- ^ Canvas size
  -> V2 CFloat -- ^ Viewport size
  -> CFloat    -- ^ Distance from origin to a projection plane
  -> V2 CFloat -- ^ Point coordinates
  -> V3 CFloat
project (V2 cw ch) (V2 vw vh) d (V2 x y) =
  let dw = vw / fromIntegral cw
      dh = vh / fromIntegral ch
   in V3 (x * dw) (y * dh) d
