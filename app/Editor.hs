module Editor (Editor (..)) where

import Data.Default
import Data.List      qualified as L
import System.Process

-- some programme able to edit a file
-- at a given filepath
newtype Editor = Editor { edit :: FilePath -> IO () }

instance Default Editor where
  def = vim
    where vim = Editor $ callProcess "vim" . L.singleton
