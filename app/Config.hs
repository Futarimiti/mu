module Config (Config(..)) where

import           Downloader (Downloader)
import           Editor     (Editor)
import           Player     (Player)

data Config = Config { editor     :: Editor
                     , player     :: Player
                     , downloader :: Downloader
                     , musicDir   :: FilePath
                     }

