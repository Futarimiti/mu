module Downloader (Downloader (..)) where

import Data.Default
import System.Process (callProcess)
import Types

-- | A programme that is able to download a song from a website
-- given by the URL to a destination
newtype Downloader = Downloader
  { download
    :: URL
    -> FilePath  -- dest
    -> IO ()
  }

instance Default Downloader where
  def = ytdlp
    where ytdlp = Downloader $ \url dest -> callProcess "yt-dlp"
            [url, "-x", "--audio-format", "mp3", "-o", dest, "-q", "--progress"]
