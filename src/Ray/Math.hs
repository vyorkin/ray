{-# LANGUAGE Strict #-}
module Ray.Math
  ( traceRay

  , module Ray.Math.Transform
  , module Ray.Math.Intersection
  ) where

import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Foreign.C.Types (CFloat)
import SDL (V3(..))

import Ray.Scene.Types (Scene(..), Camera(..))
import Ray.Color (Color)
import Ray.Lighting (calcColor)

import Ray.Math.Intersection (Intersection(..), intersect)
import Ray.Math.Transform (project)
import Ray.Math.Vector (distance)

-- | Computes the intersection of the ray with every sphere,
-- and returns the color of the sphere at the nearest intersection
-- which is inside the requested range of 't'.
traceRay :: Scene -> V3 CFloat -> (CFloat, CFloat) -> Color
traceRay Scene{ camera = Camera origin, .. } ray (tMin, tMax) =
  let ni = foldl' (\acc o -> maybe acc (nearest acc) $ intersect origin ray o) Nothing objects
   in calcColor (fromMaybe 0 ambient) lights ni
  where
    nearest i2 i1 = maybe (clamp i1) (Just . closest i1) i2
    closest i1@(Intersection _ p1 _) i2@(Intersection _ p2 _) = if p1 < p2 then i1 else i2
    inBounds i =
      let d = distance origin (iPoint i)
       in d >= tMin && d <= tMax
    clamp i | inBounds i = Just i
            | otherwise  = Nothing
{-# INLINE traceRay #-}
