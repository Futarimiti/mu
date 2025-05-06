module Main (main) where

import Commands                  (runCommandLogged)
import Config.User               (getUserConfig)
import Control.Monad.Logger      (runStderrLoggingT)
import Control.Monad.Reader      (ReaderT (runReaderT))
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import FileInfo                  qualified (parseFile)
import Global                    (Global (Global))
import Messages                  qualified (parseFile)
import Options                   (parseArgs)
import Paths_mu                  (getDataFileName)

main :: IO ()
main = do
  fi <- getDataFileName "fileinfo.dhall" >>= FileInfo.parseFile
  mess <- getDataFileName "messages.dhall" >>= Messages.parseFile
  Just c <- runMaybeT (getUserConfig fi)
  let g = Global c fi mess
  action <- runReaderT parseArgs g
  runStderrLoggingT $ runReaderT (runCommandLogged action) g
