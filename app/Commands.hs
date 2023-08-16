{-# LANGUAGE LambdaCase #-}

module Commands (runCommand, MuCommand(..)) where

import           Config  (Config)
import           Lib
import           Prelude hiding (log)

-- | Functionalities parsed from commandline args
data MuCommand = Play [SongName]  -- | Play songs sequentially
               | Shuffle          -- | Shuffle through the library
               | Update           -- | Update library
               deriving (Show, Eq, Read)

runCommand :: Config -> MuCommand -> IO ()
runCommand c = \case (Play songs) -> seqPlay songs
                     Shuffle -> shuffle c
                     Update -> update c

update :: Config -> IO ()
update = const $ log "update: TODO"

shuffle :: Config -> IO ()
shuffle = const $ log "shuffle: TODO"

seqPlay :: [SongName] -> IO ()
seqPlay = const $ log "seqPlay: TODO"

