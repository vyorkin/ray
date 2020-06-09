{-# LANGUAGE Strict #-}
module Ray.Math.Vector
  ( distance
  , dot
  , minus
  , norm
  , plus
  , scaledBy
  , signorm
  , quadrance
  ) where


import Foreign.C.Types (CFloat)
import SDL (V3(..))

distance :: V3 CFloat -> V3 CFloat -> CFloat
distance v1 v2 = norm $ v2 `minus` v1
{-# INLINE distance #-}

dot :: V3 CFloat -> V3 CFloat -> CFloat
dot (V3 x1 y1 z1) (V3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2
{-# INLINE dot #-}

minus :: V3 CFloat -> V3 CFloat -> V3 CFloat
minus (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (x2 - x1) (y2 - y1) (z2 - z1)
{-# INLINE minus #-}

norm :: V3 CFloat -> CFloat
norm (V3 x y z) = sqrt $ x*x + y*y + z*z
{-# INLINE norm #-}

plus :: V3 CFloat -> V3 CFloat -> V3 CFloat
plus (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (x2 + x1) (y2 + y1) (z2 + z1)
{-# INLINE plus #-}

scaledBy :: V3 CFloat -> CFloat -> V3 CFloat
scaledBy (V3 x y z) s = V3 (x*s) (y*s) (z*s)
{-# INLINE scaledBy #-}

signorm :: V3 CFloat -> V3 CFloat
signorm v@(V3 x y z) = V3 (x/n) (y/n) (z/n)
  where n = norm v
{-# INLINE signorm #-}

quadrance :: V3 CFloat -> CFloat
quadrance (V3 x y z) = x*x + y*y + z*z

{-# INLINE quadrance #-}
