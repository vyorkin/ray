{-# LANGUAGE DeriveAnyClass #-}

module Ray.Buffer
  ( Buffer(..)
  , unwrap
  , mkBuffer
  , toByteString
  ) where

import Foreign.C.Types (CInt)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import SDL (V2(..))

import Ray.Color (Color(..))
import qualified Ray.Color as Color

newtype Buffer = Buffer (Vector Color)

unwrap :: Buffer -> Vector Color
unwrap (Buffer vec) = vec

mkBuffer :: V2 CInt -> Color -> Buffer
mkBuffer (V2 w h) c = Buffer $ Vector.replicate (fromIntegral w * fromIntegral h) c

toByteString :: Buffer -> ByteString
toByteString = ByteString.pack
  . concatMap Color.toBytes
  . Vector.toList
  . unwrap
