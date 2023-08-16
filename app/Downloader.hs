module Downloader (Downloader(..)) where

import           Lib

-- | A programme that is able to download a song from a website
-- given by the URL to a destination
newtype Downloader = Downloader { download :: URL
                                           -> FilePath  -- dest
                                           -> IO ()
                                }

