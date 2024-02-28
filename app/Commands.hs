{-# LANGUAGE LambdaCase #-}

module Commands (runCommand, MuCommand(..)) where

import           Control.Monad.Reader (ReaderT (runReaderT))
import           Global
import           Lib
import           Play                 (playSeq, shuffle)
import           Prelude              hiding (log)
import           Update               (update)

-- | Functionalities parsed from commandline args
data MuCommand = Play [SongName]     -- | Play songs sequentially
               | Shuffle [SongName]  -- | Shuffle through the library, or specified songs
               | Update              -- | Update library
               deriving (Show, Eq, Read)

runCommand :: Global -> MuCommand -> IO ()
runCommand g = \case Play songs    -> playSeq g songs
                     Shuffle songs -> runReaderT (shuffle songs) g
                     Update        -> update g

