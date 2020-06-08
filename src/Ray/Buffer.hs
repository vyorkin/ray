module Ray.Buffer
  ( Buffer(..)
  , unwrap
  , mkBuffer
  , toByteString
  ) where

import Foreign.C.Types (CInt)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString

import SDL (V2(..))

import Ray.Color (Color)
import qualified Ray.Color as Color

newtype Buffer = Buffer [Color]

unwrap :: Buffer -> [Color]
unwrap (Buffer cs) = cs

mkBuffer :: V2 CInt -> Color -> Buffer
mkBuffer canvasSize c =
  let V2 w h = fromIntegral <$> canvasSize
   in Buffer $ replicate (w * h) c

toByteString :: Buffer -> ByteString
toByteString = ByteString.pack
  . concatMap Color.toBytes
  . unwrap
