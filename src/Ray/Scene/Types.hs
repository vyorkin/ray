module Ray.Scene.Types
  ( Scene(..)
  , Camera(..)
  , Object(..)
  , Plane(..)
  , Sphere(..)
  , mkHPlane
  , mkSphere
  ) where

import Foreign.C.Types (CFloat)
import SDL (V2, V3(..))

import Ray.Color (Color(..))

newtype Camera = Camera (V3 CFloat)
  deriving (Eq, Show)

data Scene = Scene
  { viewportSize :: !(V2 CFloat)
  -- ^ Size of a viewport
  , projPlaneZ :: !CFloat
  -- ^ Distance from origin to a projection plane
  , camera :: !Camera
  , objects :: ![Object]
  } deriving (Show)

data Sphere = Sphere
  { center :: !(V3 CFloat)
  , radius :: !CFloat
  } deriving (Eq, Show)

mkSphere :: V3 CFloat -> Color -> Object
mkSphere center = OSphere Sphere { radius = 1.0, .. }

data Plane = Plane
  { origin :: !(V3 CFloat)
  , normal :: !(V3 CFloat)
  , width  :: !Int
  , height :: !Int
  } deriving (Eq, Show)

mkHPlane :: CFloat -> Int -> Int -> Color -> Object
mkHPlane y width height = OPlane Plane {..}
  where
    origin = V3 x0 y z0
    normal = V3 0.0 1.0 0.0
    x0     = - (fromIntegral width / 2.0)
    z0     = - (fromIntegral height / 2.0)

data Object = OSphere !Sphere !Color
            | OPlane  !Plane  !Color
            deriving (Eq, Show)
