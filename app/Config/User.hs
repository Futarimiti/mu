{-# LANGUAGE OverloadedRecordDot #-}

module Config.User (getUserConfig) where

import           Config                    (Config)
import qualified Config.Parse              as Config
import           Control.Monad             (guard)
import           Control.Monad.IO.Class    (MonadIO (..))
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Control.Monad.Trans.Maybe (MaybeT)
import           FileInfo                  (FileInfo (..))
import           Prelude                   hiding (log)
import           System.Directory          (XdgDirectory (..), doesFileExist,
                                            getXdgDirectory)

getUserConfig :: MonadIO io => FileInfo -> MaybeT io Config
getUserConfig fi = do path <- liftIO $ getXdgDirectory XdgConfig fi.configFilePath
                      exists <- liftIO $ doesFileExist path
                      guard exists
                      lift $ Config.parseFile path
