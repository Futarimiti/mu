module Config.User (getUserConfig) where

import           Config                    (Config)
import           Config.Parse              (parseFile)
import           Control.Monad             (guard)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.Maybe (MaybeT)
import           FileInfo                  (configFilePath, fileinfo)
import           Prelude                   hiding (log)
import           System.Directory          (XdgDirectory (XdgConfig),
                                            doesFileExist, getXdgDirectory)

getUserConfig :: MaybeT IO Config
getUserConfig = do path <- lift (getXdgDirectory XdgConfig . configFilePath =<< fileinfo)
                   exists <- lift $ doesFileExist path
                   guard exists
                   lift $ parseFile path
