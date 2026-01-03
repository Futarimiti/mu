module YtDlp where

import Control.Monad.IO.Class
import System.Process

ytdlp :: MonadIO m => [String] -> m ()
ytdlp = liftIO . callProcess "yt-dlp"
