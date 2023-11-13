module Global (Global(..)) where

import           Config   (Config)
import           FileInfo (FileInfo)

data Global = Global { config   :: Config
                     , fileinfo :: FileInfo
                     }
