module App.Env (AppEnv (..), fromCLIOpts) where

import Config qualified
import Control.Monad.Catch    (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger
import Types

fromCLIOpts :: (MonadCatch m, MonadLogger m, MonadIO m) => Options -> m AppEnv
fromCLIOpts Options {..} = do
  conf <- Config.getWith confFile
  pure AppEnv {..}
