module Ray.Color
  ( Color(..)
  , toBytes

  , white
  , red
  , green
  , blue
  , black

  , bg
  ) where

import Control.DeepSeq (NFData(..))
import Data.Word (Word8)

data Color = Color
  { r :: !Word8
  , g :: !Word8
  , b :: !Word8
  , a :: !Word8
  } deriving (Eq, Show)

instance NFData Color where
  rnf = pure ()

toBytes :: Color -> [Word8]
toBytes (Color r g b a) = [a, b, g, r]

-- Colors

white :: Color
white = Color 255 255 255 255

red :: Color
red = Color 255 0 0 255

green :: Color
green = Color 0 255 0 255

blue :: Color
blue = Color 0 0 255 255

black :: Color
black = Color 0 0 0 255

bg :: Color
bg = white
