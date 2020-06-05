{-# LANGUAGE BangPatterns #-}
module Ray.Scene
  ( Scene(..)
  , example
  , render
  , renderTest
  , project
  , traceRay

  , module Ray.Scene.Types
  ) where

import Control.DeepSeq (force)
import Data.Foldable (foldr')
import Data.List (foldl')
import Foreign.C.Types (CInt, CFloat)
import Debug.Trace (traceM)
import SDL (V2(..), V3(..))

import Ray.Color (Color)
import qualified Ray.Color as Color
import Ray.Scene.Types (Scene(..), Camera(..), Sphere(..), mkSphere)
import Ray.Canvas (Canvas(..))
import qualified Ray.Canvas as Canvas
import qualified Ray.Math as Math (project, traceRay)

render :: Scene -> Canvas -> Canvas
render scene@Scene{..} canvas@Canvas{..} =
  let points = traceRays size scene
   in foldl' draw canvas points
  where
    draw cvs (!point, !color) = Canvas.putPixel (round <$> point) color cvs

renderTest :: Canvas -> Canvas
renderTest =
    Canvas.putPixel (V2 100 10) Color.red
  . Canvas.putPixel (V2 50 300) Color.green
  . Canvas.putPixel (V2 30 200) Color.blue

traceRays :: V2 CInt -> Scene -> [(V2 CFloat, Color)]
traceRays canvasSize scene@Scene{..} = do
  let V2 cw ch = fromIntegral <$> canvasSize
      (xl, xr) = (-cw/2, cw/2)
      (yb, yt) = (-ch/2, ch/2)
  !y <- [yb..yt - 1]
  !x <- [xl..xr - 1]
  -- traceM $ "(x, y) = " <> show (x, y)
  let !point = V2 x y
      ray = force $ project canvasSize scene point
  -- traceM $ "point = " <> show (V2 x y)
  -- traceM $ "ray = " <> show ray
  let color = force $ traceRay scene ray
  pure (point, color)

traceRay :: Scene -> V3 CFloat -> Color
traceRay Scene{camera = Camera origin, ..} ray =
  let inf = fromIntegral (maxBound :: CInt)
   in Math.traceRay spheres origin ray (projPlaneZ, inf)

-- | Projects a point to viewport.
project :: V2 CInt -> Scene -> V2 CFloat -> V3 CFloat
project canvasSize Scene{..} =
  Math.project canvasSize viewportSize projPlaneZ

-- | Example scene.
example :: Scene
example =
  let viewportSize = V2 1.0 1.0
      projPlaneZ = 1.0
      spheres = [s1, s2, s3]
      origin = V3 0.0 0.0 0.0
      camera = Camera origin
   in Scene{..}
  where
    s1 = mkSphere (V3 0 (-1) 3) Color.red
    s2 = mkSphere (V3 2 0 4) Color.blue
    s3 = mkSphere (V3 (-2) 0 4) Color.green
