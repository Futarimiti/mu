module Commands (MuCommand (..), runCommandLogged) where

import Control.Monad.Catch  (MonadMask)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadIO, MonadReader)
import Global               (Global)
import Play                 (playSeqLogged, shuffleLogged)
import Prelude              hiding (log)
import Types
import Update               (updateLogged)

-- | Functionalities parsed from commandline args
data MuCommand
  = Play [SongName]     -- | Play songs sequentially
  | Shuffle [SongName]  -- | Shuffle through the library, or specified songs
  | Update              -- | Update library
  deriving (Show, Eq, Read)

runCommandLogged
  :: (MonadIO m, MonadLogger m, MonadMask m, MonadReader Global m)
  => MuCommand
  -> m ()
runCommandLogged (Play songs)    = playSeqLogged songs
runCommandLogged (Shuffle songs) = shuffleLogged songs
runCommandLogged Update          = updateLogged
