module Config.User (getUserConfig) where

import           Config  (Config)
import           Lib
import           Prelude hiding (log)

-- | TODO
getUserConfig :: IO (Maybe Config)
getUserConfig = do log "getting user config... failed: TODO"
                   return Nothing
