module Main where

import           Commands                  (runCommand)
import           Config.User               (getUserConfig)
import           Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import           Options                   (parseArgs)

main :: IO ()
main = do action <- parseArgs
          Just userConfig <- runMaybeT getUserConfig
          runCommand userConfig action

