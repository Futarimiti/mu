module Update (update) where

import           Config
import           Lib
import           Prelude hiding (log)

update :: Config -> IO ()
update = const $ log "update: TODO"

