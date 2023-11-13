module Global (Global(..)) where

import           Config   (Config)
import           FileInfo (FileInfo)
import           Messages (Messages)

data Global = Global { config   :: Config
                     , fileinfo :: FileInfo
                     , mess     :: Messages
                     }
