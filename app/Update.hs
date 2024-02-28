{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module Update (update, updateLogged) where

import           Config                 (Config (..))
import           Control.Monad.Catch    (MonadMask)
import           Control.Monad.Except   (runExceptT)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Logger   (LoggingT (..), logErrorN, logInfoN)
import           Control.Monad.Reader   (ReaderT (runReaderT), asks)
import qualified Data.Map               as M
import           Data.Text              (unpack)
import qualified Data.Text              as T
import           Editor                 (Editor (..))
import           FileInfo               (FileInfo (..))
import           Global                 (Global (..))
import           Lib
import           Prelude                hiding (log)
import           System.Directory       (XdgDirectory (XdgData), doesFileExist,
                                         getXdgDirectory)
import           System.Exit            (exitFailure)
import           System.FilePath        ((<.>))
import           System.IO.Temp         (withSystemTempFile)
import           Update.Manage          (manageSongs, manageSongsLogged)
import           Update.Parse           (decodeFile, encodeFile)

update :: Global -> IO ()
update g@(Global {..}) = do let fi = fileinfo
                            let tempFileTemplate = fi.updateFilename <.> fi.updateFileExt
                            withSystemTempFile tempFileTemplate $ \f _ -> do
                              mapPath <- getXdgDirectory XdgData fi.serialiseDataPath
                              hasSerialisedData <- doesFileExist mapPath
                              oldMap <- if hasSerialisedData then deserialiseMap mapPath
                                                             else return M.empty
                              encodeFile f oldMap
                              _ <- config.editor.edit f
                              e <- runExceptT $ decodeFile f
                              case e of
                                Left err -> do
                                  let msg = unpack err
                                  log $ unlines ["Syntax error in parsing yaml:", msg, "Failed, nothing changed"]
                                  exitFailure
                                Right newMap -> do
                                  let changes = oldMap `compareTo` newMap
                                  log (prettyPrintChanges changes)
                                  manage <- runReaderT manageSongs g
                                  manage changes
                                  serialiseMap newMap =<< getXdgDirectory XdgData fi.serialiseDataPath

updateLogged :: (MonadIO io, MonadMask io) => ReaderT Global (LoggingT io) ()
updateLogged = do fi <- asks fileinfo
                  editor <- asks (editor . config)
                  let tempFileTemplate = fi.updateFilename <.> fi.updateFileExt
                  withSystemTempFile tempFileTemplate $ \f _ -> do
                    mapPath <- liftIO $ getXdgDirectory XdgData fi.serialiseDataPath
                    hasSerialisedData <- liftIO $ doesFileExist mapPath
                    oldMap <- if hasSerialisedData then deserialiseMap mapPath else return M.empty
                    encodeFile f oldMap
                    liftIO $ editor.edit f
                    decodeResult <- runExceptT $ decodeFile f
                    case decodeResult of
                      Left err -> logErrorN $ T.unlines ["Syntax error in parsing yaml:", err, "Failed, nothing changed"]
                      Right newMap -> do
                        let changes = oldMap `compareTo` newMap
                        logInfoN $ T.pack $ prettyPrintChanges changes
                        manageSongsLogged changes
                        liftIO $ serialiseMap newMap =<< getXdgDirectory XdgData fi.serialiseDataPath
