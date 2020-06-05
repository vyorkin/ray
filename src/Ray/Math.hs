module Ray.Math
  ( traceRay
  , intersect

  , module Ray.Math.V3
  , module Ray.Math.Intersection
  ) where

import Data.Maybe (mapMaybe)
import Foreign.C.Types (CFloat)
import SDL (V3(..))

import Ray.Scene.Types (Sphere(..))
import Ray.Color (Color)

import Ray.Math.V3 ((<.>))
import Ray.Math.Intersection (Intersection(..))
import qualified Ray.Math.Intersection as Intersection

-- | Computes the intersection of the ray with every sphere,
-- and returns the color of the sphere at the nearest intersection
-- which is inside the requested range of 't'.
traceRay :: [Sphere] -> V3 CFloat -> V3 CFloat -> (CFloat, CFloat) -> Color
traceRay spheres origin ray bounds =
  let is = mapMaybe (intersect origin ray) spheres
  in Intersection.toColor $ foldr nearest Nothing is
  where
    nearest i1 = maybe (clamp i1) (closest i1)
    closest i1 i2 =
      let (t1, t2) = (Intersection.point i1, Intersection.point i2)
       in clamp (if t1 < t2 then i1 else i2)
    clamp i
      | Intersection.inBounds i bounds = Just i
      | otherwise = Nothing

-- | Returns two points of intersection between ray and sphere.
-- * O - origin
-- * D - ray
intersect :: V3 CFloat -> V3 CFloat -> Sphere -> Maybe Intersection
intersect origin ray s =
  let
    c  = center s
    r  = radius s
    oc = origin - c
    k1 = ray <.> ray
    k2 = 2 * oc <.> ray
    k3 = oc <.> oc - r * r
    d  = k2 * k2 - 4 * k1 * k3
    t1 = (-k2 + sqrt d) / 2 * k1
    t2 = (-k2 - sqrt d) / 2 * k2
   in
    if d < 0
    then Nothing
    else Just $ Intersection s (min t1 t2)
