{-# LANGUAGE Strict #-}
module Ray.Lighting
  ( calcColor
  ) where

import Foreign.C.Types (CFloat)
import SDL (V3(..), V4(..))

import Ray.Scene.Types (Light(..))
import qualified Ray.Scene.Types as Light (intensity)
import Ray.Math.Intersection (Intersection(..))
import Ray.Math.Vector (dot, minus, norm, plus, scaledBy)
import Ray.Color (Color)
import qualified Ray.Color as Color

calcColor :: CFloat -> [Light] -> Maybe Intersection -> Color
calcColor ambient lights = maybe Color.bg (applyLights ambient lights)

applyLights :: CFloat -> [Light] -> Intersection -> Color
applyLights ambient lights Intersection{..} = transform iColor
  where
    transform (V4 r g b a) =
      let intensity = ambient + computeAll iPoint iNormal lights
          rint = round $ recip intensity
          r1 = r `div` rint
          g1 = g `div` rint
          b1 = b `div` rint
          a1 = a `div` rint
       in V4 r1 g1 b1 a1

computeAll :: V3 CFloat -> V3 CFloat -> [Light] -> CFloat
computeAll p n = sum . map (compute p n)

compute :: V3 CFloat -> V3 CFloat -> Light -> CFloat
compute p n light =
  let l = dir light
      d = n `dot` l
   in clamp d $ (Light.intensity light * d) / (norm n * norm l)
  where
    dir (PointLight _ pos) = pos `minus` p
    dir (DirectionalLight _ d) = d

    clamp d x
      | d > 0 = x
      | otherwise = 0
