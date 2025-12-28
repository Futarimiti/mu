{-# LANGUAGE OverloadedStrings #-}

module Config.User (getUserConfig) where

import Config                    (Config)
import Config.Parse              qualified as Config
import Control.Monad
import Control.Monad.IO.Class    (MonadIO (..))
import Control.Monad.Logger
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Maybe (MaybeT)
import Data.String.Interpolate   (i)
import FileInfo                  (FileInfo (..))
import System.Directory          (XdgDirectory (..), doesFileExist, getXdgDirectory)

getUserConfig :: (MonadLogger m, MonadIO m) => FileInfo -> MaybeT m Config
getUserConfig fi = do
  path <- do
    $logDebug "getting path to user config"
    liftIO $ getXdgDirectory XdgConfig fi.configFilePath
  exists <- do
    $logDebug "getting status of user config"
    liftIO $ doesFileExist path
  unless exists $ do
    $logError [i|user config does not exist at #{path}|]
    fail "failed to get user config"
  lift $ Config.parseFile path
