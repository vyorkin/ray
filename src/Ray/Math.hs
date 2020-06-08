module Ray.Math
  ( traceRay

  , module Ray.Math.Transform
  , module Ray.Math.Intersection
  ) where

import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Foreign.C.Types (CFloat)
import SDL (V3(..))

import Ray.Scene.Types (Object(..))
import Ray.Color (Color)

import Ray.Math.Intersection (Intersection(..), point, inBounds, intersect)
import qualified Ray.Math.Intersection as Intersection
import Ray.Math.Transform (project)

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
