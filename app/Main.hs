module Main (main) where

import Commandline
import Data.String.Interpolate (i)

main :: IO ()
main = do
  options <- Commandline.parseArgs
  putStrLn [i|Got CLI options: #{options}|]
