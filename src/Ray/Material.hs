module Ray.Material
  ( Material(..)
  ) where

import Foreign.C.Types (CFloat)
import Ray.Color (Color)

data Material = Material
  { color :: !Color
  , specular :: !CFloat
  } deriving (Eq, Show)
