module Ray.Math
  ( traceRay

  , module Ray.Math.Transform
  , module Ray.Math.Intersection
  ) where

import Data.Function (on)
import Data.List (foldl', minimumBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Foreign.C.Types (CFloat)
import SDL (V3(..), distance)

import Ray.Scene.Types (Scene(..), Camera(..))
import Ray.Color (Color)
import Ray.Lighting (calcColor)

import Ray.Math.Intersection (Intersection(..), intersect)
import Ray.Math.Transform (project)

-- | Computes the intersection of the ray with every sphere,
-- and returns the color of the sphere at the nearest intersection
-- which is inside the requested range of 't'.
traceRay :: Scene -> V3 CFloat -> (CFloat, CFloat) -> Color
traceRay Scene{ camera = Camera origin, .. } ray (tMin, tMax) =
  let is = mapMaybe (intersect origin ray) objects
      ni = foldl' nearest Nothing is
   in calcColor (fromMaybe 0 ambient) lights ni
  where
    nearest i2 i1 = maybe (clamp i1) (Just . closest i1) i2
    closest i1 i2 = minimumBy (compare `on` iPoint) [i1, i2]
    inBounds i =
      let d = distance origin (iPoint i)
       in d >= tMin && d <= tMax
    clamp i | inBounds i = Just i
            | otherwise  = Nothing
