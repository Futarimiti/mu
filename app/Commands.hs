{-# LANGUAGE LambdaCase #-}

module Commands (runCommand, MuCommand(..)) where

import           Config  (Config)
import           Lib
import           Play    (playSeq, shuffle)
import           Prelude hiding (log)
import           Update  (update)

-- | Functionalities parsed from commandline args
data MuCommand = Play [SongName]  -- | Play songs sequentially
               | Shuffle          -- | Shuffle through the library
               | Update           -- | Update library
               deriving (Show, Eq, Read)

runCommand :: Config -> MuCommand -> IO ()
runCommand c = \case (Play songs) -> playSeq c songs
                     Shuffle -> shuffle c
                     Update -> update c

