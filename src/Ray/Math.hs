{-# LANGUAGE BangPatterns #-}
module Ray.Math
  ( traceRay
  , intersect
  , project

  , module Ray.Math.V3
  , module Ray.Math.Intersection
  ) where

import Control.Monad (guard)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Foreign.C.Types (CInt, CFloat)
import SDL (V2(..), V3(..), distance, dot, norm)

import Ray.Scene.Types (Object(..), Circle(..), Plane(..), Sphere(..), Square(..))
import Ray.Color (Color)

import Ray.Math.V3 ((<.>))
import Ray.Math.Intersection (Intersection(..), point, inBounds)
import qualified Ray.Math.Intersection as Intersection

-- | Computes the intersection of the ray with every sphere,
-- and returns the color of the sphere at the nearest intersection
-- which is inside the requested range of 't'.
traceRay :: [Object] -> V3 CFloat -> V3 CFloat -> (CFloat, CFloat) -> Color
traceRay objects origin ray _bounds =
  let is = mapMaybe (intersect origin ray) objects
   in Intersection.toColor $ foldl' nearest Nothing is
  where
    nearest i2 i1 = maybe (Just i1) (closest i1) i2
    closest i1 i2 =
      let (t1, t2) = (point i1, point i2)
          i = if t1 < t2 then i1 else i2
       in Just i
    -- clamp i
    --   | inBounds i bounds = Just i
    --   | otherwise = Nothing

-- | Returns two points of intersection between ray and sphere.
-- * O - origin
-- * D - ray
intersect :: V3 CFloat -> V3 CFloat -> Object -> Maybe Intersection
intersect origin ray (OSphere s color) =
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
    else Just $ Intersection color (min t1 t2)
intersect start ray (OPlane plane color) =
  intersectPlane start ray color plane
intersect start ray (OCircle (Circle plane radius) color) = do
  i@(Intersection _ len) <- intersectPlane start ray color plane
  let r1 = distance (origin plane) (start + fmap (* len) ray)
  guard (r1 <= radius)
  pure i
intersect start ray (OSquare Square {..} color) = do
  i@(Intersection _ len) <- intersectPlane start ray color plane
  let inPlane = (start + fmap (* len) ray) - origin plane
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
