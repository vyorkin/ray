module Main (main) where

import Ray (run)
import Ray.Config (Config(..))

main :: IO ()
main = do
  let
    width = 600
    height = 600
  run Config{..}
