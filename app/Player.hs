module Player (Player(..), afplayHQ) where

import           System.Process (callProcess)

-- | A programme that is able to play some audio
newtype Player = Player { play :: FilePath -> IO () }

-- | afplay, the default macOS music player.
-- High quality option set.
afplayHQ :: Player
afplayHQ = Player { play = \f -> callProcess "afplay" ["--rQuality", "1", f] }
