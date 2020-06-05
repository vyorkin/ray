module Ray.Math.V3
  ( (<.>)
  ) where

import SDL (V3(..))

infixl 7 <.>

-- | Dot product
(<.>) :: Num a => V3 a -> V3 a -> a
(<.>) (V3 x y z) (V3 u v w) = x * u + y * v + z * w
