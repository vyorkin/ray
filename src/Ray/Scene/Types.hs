module Ray.Scene.Types
  ( Scene(..)
  , Light(..)
  , Camera(..)
  , Object(..)
  , Circle(..)
  , Plane(..)
  , Sphere(..)
  , Square(..)
  , intensity
  , mkHPlane
  , mkHSquare
  , mkSphere
  ) where

import Foreign.C.Types (CFloat)
import SDL (V2, V3(..))

import Ray.Color (Color)

newtype Camera = Camera (V3 CFloat)
  deriving (Eq, Show)

data Light
  = PointLight       !CFloat !(V3 CFloat)
  | DirectionalLight !CFloat !(V3 CFloat)
  deriving (Eq, Show)

intensity :: Light -> CFloat
intensity (PointLight i _) = i
intensity (DirectionalLight i _) = i

data Scene = Scene
  { viewportSize :: !(V2 CFloat)
  -- ^ Size of a viewport
  , projPlaneZ :: !CFloat
  -- ^ Distance from origin to a projection plane
  , camera :: !Camera
  , objects :: ![Object]
  , lights :: ![Light]
  , ambient :: !(Maybe CFloat)
  } deriving (Show)

data Sphere = Sphere
  { center :: !(V3 CFloat)
  , radius :: !CFloat
  } deriving (Eq, Show)

data Plane = Plane
  { origin :: !(V3 CFloat)
  , normal :: !(V3 CFloat)
  } deriving (Eq, Show)

-- | Circle consists of a plane and a
-- radius from the plane's origin
data Circle = Circle !Plane !CFloat
  deriving (Eq, Show)

data Square = Square
  { plane  :: !Plane
  , xdir   :: !(V3 CFloat)
  -- ^ X-Axis direction
  , width  :: !CFloat
  , height :: !CFloat
  } deriving (Eq, Show)

data Object
  = OSphere !Sphere !Color
  | OPlane  !Plane  !Color
  | OCircle !Circle !Color
  | OSquare !Square !Color
  deriving (Eq, Show)

mkSphere :: V3 CFloat -> Color -> Object
mkSphere center = OSphere Sphere { radius = 1, .. }

mkHPlane :: CFloat -> Color -> Object
mkHPlane y = OPlane Plane {..}
  where
    origin = V3 0 y 0
    normal = V3 0 1 0

mkHSquare :: Bool -> CFloat -> CFloat -> CFloat -> Color -> Object
mkHSquare up y width height = OSquare Square {..}
  where
    origin = V3 x0 y z0
    normal = V3 0 (if up then 1 else -1) 0
    plane  = Plane {..}
    xdir   = V3 1 0 0
    x0     = - width / 2
    z0     = - height / 2
