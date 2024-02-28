module Main (main) where

import           Commands                  (runCommand)
import           Config.User               (getUserConfig)
import           Control.Monad.Reader      (ReaderT (runReaderT))
import           Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import           FileInfo                  (fileinfo)
import           Global                    (Global (Global))
import           Messages                  (messages)
import           Options                   (parseArgs)

main :: IO ()
main = do fi <- fileinfo
          mess <- messages
          Just c <- runMaybeT getUserConfig
          let g = Global c fi mess
          action <- runReaderT parseArgs g
          runCommand g action
