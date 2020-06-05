module Main (main) where

import Ray (run)
import Ray.Config (Config(..))

main :: IO ()
main = do
  let
    width = 640
    height = 640
  run Config{..}
