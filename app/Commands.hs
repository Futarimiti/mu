module Commands (MuCommand(..)) where

import           Lib

-- | Functionalities parsed from commandline args
data MuCommand = Play [SongName]  -- | Play songs sequentially
               | Shuffle          -- | Shuffle through the library
               | Update           -- | Update library
               deriving (Show, Eq, Read)
