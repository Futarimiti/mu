module Main where

import           Commands                   (runCommand)
import           Config.User                (getUserConfig)
import           Control.Monad.Trans.Maybe  (MaybeT (runMaybeT))
import           Control.Monad.Trans.Reader (ReaderT (runReaderT))
import           Options                    (parseArgs)

main :: IO ()
main = do Just userConfig <- runMaybeT getUserConfig
          action <- runReaderT parseArgs userConfig
          runCommand userConfig action

