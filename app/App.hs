module App (runWithCLIOpts) where

import App.Env
import App.Env                 qualified as AppEnv
import Control.Exception.Safe
import Control.Lens
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Generics.Labels ()
import Data.String.Interpolate (i)
import System.Environment      qualified as Env
import System.Exit
import Types

type AppM m = (MonadReader AppEnv m, MonadIO m, MonadLogger m)

app :: AppM m => m ()
app = do
  args <- liftIO Env.getArgs
  $logDebug [i|CLI args: #{args}|]
  options <- ask
  $logDebug [i|CLI options: #{options}|]
  config <- view #conf
  $logDebug [i|User config: #{config}|]
  liftIO $ putStrLn "imagine song playing"

runWithCLIOpts :: Options -> IO ()
runWithCLIOpts opts = withLogging . dieOnErrors $ do
  env <- AppEnv.fromCLIOpts opts
  runReaderT app env
  where
    verbose = opts ^. #verbose
    withLogging = runStderrLoggingT . if verbose then id else filterUserVisibleLogs
    filterUserVisibleLogs = errorAndCustomOnly
    errorAndCustomOnly = filterLogger $ \cases
      _ LevelError -> True
      _ (LevelOther _) -> True
      _ _ -> False
    dieOnErrors = handleIf (not . isAsyncException) $ \(SomeException e) -> do
      $logError [i|fatal error: #{displayException e}|]
      liftIO $ if verbose then exitFailure else die "failed (run with -V for details)"
