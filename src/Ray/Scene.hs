{-# LANGUAGE BangPatterns #-}
module Ray.Scene
  ( Scene(..)

  , example
  , render
  , project
  , traceRay

  , module Ray.Scene.Types
  ) where

import Foreign.C.Types (CInt, CFloat)
import SDL (V2(..), V3(..))

import Ray.Color (Color(..))
import qualified Ray.Color as Color
import Ray.Buffer (Buffer(..))
import Ray.Scene.Types (Scene(..), Camera(..), Object(..), Circle(..), Plane(..), Sphere(..), Square(..), mkHPlane, mkHSquare, mkSphere)
import Ray.Canvas (Canvas(..))
import qualified Ray.Math as Math (project, traceRay)

render :: Scene -> Canvas -> Canvas
render scene@Scene{..} Canvas{..} =
  let colors = traceRays size scene
   in Canvas { buffer = Buffer colors, .. }

traceRays :: V2 CInt -> Scene -> [Color]
traceRays canvasSize scene@Scene{..} = do
  let V2 cw ch = fromIntegral <$> canvasSize
      (xl, xr) = (-cw/2, cw/2)
      (yb, yt) = (-ch/2, ch/2)
  y <- [yb..yt - 1]
  x <- [xl..xr - 1]
  let ray = project canvasSize scene (V2 x (-y))
  pure $ traceRay scene ray

traceRay :: Scene -> V3 CFloat -> Color
traceRay Scene{camera = Camera origin, ..} ray =
  let inf = fromIntegral (maxBound :: CInt)
   in Math.traceRay objects origin ray (projPlaneZ, inf)

-- | Projects a point to viewport.
project :: V2 CInt -> Scene -> V2 CFloat -> V3 CFloat
project canvasSize Scene{..} =
  Math.project canvasSize viewportSize projPlaneZ

-- | Example scene.
example :: Scene
example =
  let viewportSize = V2 1.0 1.0
      projPlaneZ = 1.0
      objects = [s1, s2, s3, c1, sq1]
      origin = V3 0.0 0.0 0.0
      camera = Camera origin
   in Scene{..}
  where
    s1 = mkSphere (V3 0 (-1) 8) Color.red
    s2 = mkSphere (V3 2 0 10) Color.blue
    s3 = mkSphere (V3 (-2) 0 10) Color.green
    color = Color 64 128 128 255
    p1@(OPlane plane _) = mkHPlane (3.0) color
    c1 = OCircle  (Circle plane 10) color
    sq1 = mkHSquare False (-2.0) 10 10 (Color 128 128 64 255)
