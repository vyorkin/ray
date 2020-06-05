module Ray.Math.Intersection
  ( Intersection(..)

  , sphere
  , point
  , inBounds
  , toColor
  ) where

import Foreign.C.Types (CFloat)

import Ray.Scene.Types (Sphere(..))
import Ray.Color (Color)
import qualified Ray.Color as Color

-- | Nearest intersection point between ray and sphere.
data Intersection = Intersection !Sphere !CFloat

sphere :: Intersection -> Sphere
sphere (Intersection s _) = s

point :: Intersection -> CFloat
point (Intersection _ p) = p

inBounds :: Intersection -> (CFloat, CFloat) -> Bool
inBounds i (tMin, tMax) = let t = point i in t >= tMin && t <= tMax

toColor :: Maybe Intersection -> Color
toColor = maybe Color.bg (color . sphere)
