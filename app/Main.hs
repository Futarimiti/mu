module Main where

import           Commands        (runCommand)
import           Config.Defaults (defaults)
import           Config.User     (getUserConfig)
import           Data.Maybe      (fromMaybe)
import           Options         (parseArgs)

main :: IO ()
main = do action <- parseArgs
          userConfig <- getUserConfig
          defaultConfig <- defaults
          let config = fromMaybe defaultConfig userConfig
          runCommand config action

