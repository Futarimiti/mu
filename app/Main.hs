module Main (main) where

import App qualified
import Commandline qualified as CLI

main :: IO ()
main = do
  options <- CLI.parseArgs
  App.runWithCLIOpts options
