module Ray.Buffer
  ( Buffer(..)
  , mkBuffer
  , toByteString
  ) where

import Foreign.C.Types (CInt)
import Data.ByteString (ByteString)
import Data.Word (Word32)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector (replicate)
import Data.Vector.Storable.ByteString (vectorToByteString)
import SDL (V2(..))

newtype Buffer = Buffer (Vector Word32)

mkBuffer :: V2 CInt -> Word32 -> Buffer
mkBuffer (V2 w h) c = Buffer $ Vector.replicate (fromIntegral w * fromIntegral h) c

toByteString :: Buffer -> ByteString
toByteString (Buffer vec) = vectorToByteString vec
