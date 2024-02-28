{-# LANGUAGE OverloadedRecordDot #-}

module Config.User (getUserConfig, getUserConfig') where

import           Config                    (Config)
import           Config.Parse              (parseFile)
import qualified Config.Parse              as Config
import           Control.Monad             (guard)
import           Control.Monad.IO.Class    (MonadIO (..))
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Control.Monad.Trans.Maybe (MaybeT)
import           FileInfo                  (FileInfo (..), fileinfo)
import           Prelude                   hiding (log)
import           System.Directory          (XdgDirectory (..), doesFileExist,
                                            getXdgDirectory)

{-# DEPRECATED getUserConfig "Use getUserConfig' instead" #-}
getUserConfig :: MonadIO io => MaybeT io Config
getUserConfig = do path <- lift (liftIO . getXdgDirectory XdgConfig . configFilePath =<< fileinfo)
                   exists <- lift . liftIO $ doesFileExist path
                   guard exists
                   lift $ parseFile path

getUserConfig' :: MonadIO io => FileInfo -> MaybeT io Config
getUserConfig' fi = do path <- liftIO $ getXdgDirectory XdgConfig fi.configFilePath
                       exists <- liftIO $ doesFileExist path
                       guard exists
                       lift $ Config.parseFile path
