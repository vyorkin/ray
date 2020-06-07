module Ray.Math.Intersection
  ( Intersection(..)
  , color
  , point
  , inBounds
  , toColor
  ) where

import Foreign.C.Types (CFloat)

import Ray.Color (Color)
import qualified Ray.Color as Color

-- | Nearest intersection point between ray and sphere.
data Intersection = Intersection !Color !CFloat

color :: Intersection -> Color
color (Intersection c _) = c

point :: Intersection -> CFloat
point (Intersection _ p) = p

inBounds :: Intersection -> (CFloat, CFloat) -> Bool
inBounds i (tMin, tMax) =
  let t = point i
   in t >= tMin && t <= tMax

toColor :: Maybe Intersection -> Color
toColor = maybe Color.bg color
