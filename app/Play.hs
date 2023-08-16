module Play (playSeq, shuffle) where

import           Config  (Config)
import           Lib
import           Prelude hiding (log)

playSeq :: Config -> [SongName] -> IO ()
playSeq _ = const $ log "seqPlay: TODO"

shuffle :: Config -> IO ()
shuffle = const $ log "shuffle: TODO"
