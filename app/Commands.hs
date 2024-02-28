module Commands (runCommandLogged, MuCommand(..)) where

import           Control.Monad.Catch  (MonadMask)
import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Reader (MonadIO, ReaderT (..))
import           Global               (Global)
import           Lib                  (SongName)
import           Play                 (playSeqLogged, shuffleLogged)
import           Prelude              hiding (log)
import           Update               (updateLogged)

-- | Functionalities parsed from commandline args
data MuCommand = Play [SongName]     -- | Play songs sequentially
               | Shuffle [SongName]  -- | Shuffle through the library, or specified songs
               | Update              -- | Update library
               deriving (Show, Eq, Read)

runCommandLogged :: (MonadIO io, MonadMask io) => MuCommand -> ReaderT Global (LoggingT io) ()
runCommandLogged (Play songs)    = playSeqLogged songs
runCommandLogged (Shuffle songs) = shuffleLogged songs
runCommandLogged Update          = updateLogged
