module Main (main) where

import Commandline             as CLI
import Config qualified
import Control.Exception.Safe
import Control.Lens
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Generics.Labels ()
import Data.String.Interpolate (i)
import System.Environment      qualified as Env
import System.Exit

main :: IO ()
main = do
  options <- CLI.parseArgs
  let
    verbose = options ^. #verbose
    withLogging = runStderrLoggingT .
        if verbose then id else filterUserVisibleLogs
  withLogging . dieOnErrors (options ^. #verbose) $ do
    args <- liftIO Env.getArgs
    $logDebug [i|CLI args: #{args}|]
    $logDebug [i|CLI options: #{options}|]
    config <- Config.getWith (options ^. #confFile)
    $logDebug [i|User config: #{config}|]
    liftIO $ putStrLn "imagine song playing"
  where
    filterUserVisibleLogs = errorAndCustomOnly
    errorAndCustomOnly = filterLogger $ \cases
      _ LevelError -> True
      _ (LevelOther _) -> True
      _ _ -> False
    dieOnErrors
      :: (MonadCatch m, MonadIO m, MonadLogger m)
      => Bool  -- used verbose?
      -> m a
      -> m a
    dieOnErrors verbose = handleIf (not . isAsyncException) $ \(SomeException e) -> do
      $logError [i|fatal error: #{displayException e}|]
      liftIO $ if verbose then
        exitFailure
      else
        die "failed (run with -V for details)"
