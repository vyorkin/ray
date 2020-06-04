module Ray.Config
  ( Config(..)
  ) where

data Config = Config
  { width :: !Int
  , height :: !Int
  } deriving (Eq, Show)
