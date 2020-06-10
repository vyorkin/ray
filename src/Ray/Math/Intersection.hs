{-# LANGUAGE Strict #-}
module Ray.Math.Intersection
  ( Intersection(..)
  , intersect
  , intersectSphere
  , intersectPlane
  ) where

import Control.Monad (guard)
import Foreign.C.Types (CFloat)
import SDL (V3(..))

import Ray.Scene.Types (Object(..), Sphere(..), Plane(..), Circle(..), Square(..))
import Ray.Color (Color)
import Ray.Math.Vector (distance, dot, minus, norm, plus, scaledBy, signorm, quadrance)

-- | Nearest intersection and color of a point.
data Intersection = Intersection
  { iColor  :: !Color
  , iPoint  :: !(V3 CFloat)
  , iNormal :: !(V3 CFloat)
  } deriving (Eq, Show)

step :: V3 CFloat -> V3 CFloat -> CFloat -> V3 CFloat
step o ray t = o `plus` (ray `scaledBy` t)

-- | Returns intersection between ray and scene object.
intersect :: V3 CFloat -> V3 CFloat -> Object -> Maybe Intersection
intersect start ray (OSphere s color) =
  intersectSphere start ray s color
intersect start ray (OPlane plane color) =
  intersectPlane start ray color plane
intersect start ray (OCircle (Circle plane radius) color) = do
  i@(Intersection {..}) <- intersectPlane start ray color plane
  let r1 = distance (origin plane) iPoint
  guard (r1 <= radius)
  pure i
intersect start ray (OSquare Square {..} color) = do
  i@(Intersection _ p _) <- intersectPlane start ray color plane
  let inPlane = p `minus` origin plane
      xdist = xdir `dot` inPlane
      ydist = norm $ inPlane `minus` (xdir `scaledBy` xdist)
  guard (xdist <= width && ydist <= height)
  pure i

-- | Private plane intersection helper.
intersectPlane :: V3 CFloat -> V3 CFloat -> Color -> Plane -> Maybe Intersection
intersectPlane start ray color Plane {..} =
  let
    prod = normal `dot` ray
    w    = start `minus` origin
    len  = (- (normal `dot` w)) / prod
    p    = step start ray len
  in
    if prod >= 0
    then Nothing
    else Just $ Intersection color p normal

intersectSphere :: V3 CFloat -> V3 CFloat -> Sphere -> Color -> Maybe Intersection
intersectSphere start ray s color =
  let
    c  = center s
    r  = radius s
    oc = start `minus` c
    k1 = quadrance ray
    k2 = 2 * oc `dot` ray
    k3 = quadrance oc - r * r
    d  = k2 * k2 - 4 * k1 * k3
    t1 = (-k2 + sqrt d) / 2 * k1
    t2 = (-k2 - sqrt d) / 2 * k2
    t  = min t1 t2
    p  = step start ray t
    n  = signorm $ p `minus` c
   in
    if d < 0
    then Nothing
    else Just $ Intersection color p n
