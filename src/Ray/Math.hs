module Ray.Math
  ( traceRay

  , module Ray.Math.Transform
  , module Ray.Math.Intersection
  ) where

import Data.List (foldl', minimumBy)
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
traceRay objects origin ray bounds =
  let is = mapMaybe (intersect origin ray) objects
   in Intersection.toColor $ foldl' nearest Nothing is
  where
    nearest i2 i1 = maybe (clamp i1) (Just . closest i1) i2
    closest i1 i2 = minimumBy (\x1 x2 -> compare (point x1) (point x2)) [i1, i2]
    clamp :: Intersection -> Maybe Intersection
    clamp i | inBounds i bounds = Just i
            | otherwise = Nothing
