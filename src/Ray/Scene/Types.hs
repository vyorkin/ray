module Ray.Scene.Types
  ( Scene(..)
  , Camera(..)
  , Sphere(..)
  , mkSphere
  ) where

import Foreign.C.Types (CFloat)
import SDL (V2, V3)

import Ray.Color (Color(..))

newtype Camera = Camera (V3 CFloat)
  deriving (Eq, Show)

data Scene = Scene
  { viewportSize :: !(V2 CFloat)
  -- ^ Size of the viewport
  , projPlaneZ :: !CFloat
  -- ^ Distance from origin to a projection plane
  , camera :: !Camera
  , spheres :: ![Sphere]
  } deriving (Show)

data Sphere = Sphere
  { center :: !(V3 CFloat)
  , radius :: !CFloat
  , color  :: !Color
  } deriving (Eq, Show)

mkSphere :: V3 CFloat -> Color -> Sphere
mkSphere center color = Sphere { radius = 1.0, .. }
