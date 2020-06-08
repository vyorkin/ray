module Ray.Color
  ( Color
  , toBytes

  , white
  , red
  , green
  , blue
  , black

  , bg
  ) where

import SDL (V4(..))
import Data.Word (Word8)

type Color = V4 Word8

toBytes :: Color -> [Word8]
toBytes (V4 r g b a) = [a, b, g, r]

-- Colors

white :: Color
white = V4 255 255 255 255

red :: Color
red = V4 255 0 0 255

green :: Color
green = V4 0 255 0 255

blue :: Color
blue = V4 0 0 255 255

black :: Color
black = V4 0 0 0 255

bg :: Color
bg = white
