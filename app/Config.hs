module Config (Config (..)) where

import Data.Default
import Downloader   (Downloader)
import Editor       (Editor)
import Player       (Player)

data Config = Config
  { editor     :: Editor
  , player     :: Player
  , downloader :: Downloader
  , musicDir   :: FilePath
  }

instance Default Config where
  def = Config def def def "/"  -- XXX
