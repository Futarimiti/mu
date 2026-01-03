module MPV (mpv) where

import Control.Monad.IO.Class
import System.Process

mpv :: MonadIO m => [String] -> m ()
mpv = liftIO . callProcess "mpv"
