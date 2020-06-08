module Ray.Math.Intersection
  ( Intersection(..)
  , point
  , inBounds
  , toColor
  , intersect
  , intersectSphere
  , intersectPlane
  ) where

import Control.Monad (guard)
import Foreign.C.Types (CFloat)
import SDL (V3(..), dot, distance, norm)

import Ray.Scene.Types (Object(..), Sphere(..), Plane(..), Circle(..), Square(..))
import Ray.Color (Color)
import qualified Ray.Color as Color

-- | Nearest intersection point between ray and sphere.
data Intersection = Intersection !Color !CFloat

point :: Intersection -> CFloat
point (Intersection _ p) = p

inBounds :: Intersection -> (CFloat, CFloat) -> Bool
inBounds i (tMin, tMax) =
  let t = point i
   in t >= tMin && t <= tMax

toColor :: Maybe Intersection -> Color
toColor = maybe Color.bg (\(Intersection c _) -> c)

-- | Returns two points of intersection between ray and sphere.
-- * O - origin
-- * D - ray
intersect :: V3 CFloat -> V3 CFloat -> Object -> Maybe Intersection
intersect start ray (OSphere s color) =
  intersectSphere start ray s color
intersect start ray (OPlane plane color) =
  intersectPlane start ray color plane
intersect start ray (OCircle (Circle plane radius) color) = do
  i@(Intersection _ len) <- intersectPlane start ray color plane
  let r1 = distance (origin plane) (start + fmap (* len) ray)
  guard (r1 <= radius)
  pure i
intersect start ray (OSquare Square {..} color) = do
  i@(Intersection _ len) <- intersectPlane start ray color plane
  let inPlane = start + fmap (* len) ray - origin plane
      xdist = xdir `dot` inPlane
      ydist = norm $ inPlane - fmap (* xdist) xdir
  guard (xdist <= width && ydist <= height)
  pure i

-- | Private plane intersection helper.
intersectPlane :: V3 CFloat -> V3 CFloat -> Color -> Plane -> Maybe Intersection
intersectPlane start ray color Plane {..} =
  let
    prod = normal `dot` ray
    w    = start - origin
    len  = (- (normal `dot` w)) / (normal `dot` ray)
  in
    if prod >= 0
    then Nothing
    else Just $ Intersection color len

intersectSphere :: V3 CFloat -> V3 CFloat -> Sphere -> Color -> Maybe Intersection
intersectSphere start ray s color =
  let
    c  = center s
    r  = radius s
    oc = start - c
    k1 = dot ray ray
    k2 = 2 * dot oc ray
    k3 = dot oc oc - r * r
    d  = k2 * k2 - 4 * k1 * k3
    t1 = (-k2 + sqrt d) / 2 * k1
    t2 = (-k2 - sqrt d) / 2 * k2
   in
    if d < 0
    then Nothing
    else Just $ Intersection color (min t1 t2)
