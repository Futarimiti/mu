{-# LANGUAGE OverloadedRecordDot #-}

module Update (updateLogged) where

import           Config                 (Config (..))
import           Control.Monad.Catch    (MonadMask, MonadThrow (..))
import           Control.Monad.Except   (runExceptT)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Logger   (MonadLogger, logErrorN, logInfoN)
import           Control.Monad.Reader   (MonadReader (ask),
                                         ReaderT (runReaderT))
import qualified Data.Text              as Text (unlines)
import           Editor                 (Editor (..))
import           FileInfo               (FileInfo (..))
import           Global                 (Global (..))
import           Prelude                hiding (log)
import           System.Directory       (XdgDirectory (XdgData), doesFileExist,
                                         getXdgDirectory)
import           System.FilePath        ((<.>))
import           System.IO.Temp         (withSystemTempFile)
import           Update.Changes         (compareTo, noChange,
                                         prettyPrintChanges)
import           Update.Manage          (manageSongsLogged)
import           Update.Parse           (decodeFile', encodeFile')
import           Update.Serialisation   (deserialiseMap, serialiseMap)

updateLogged ::
  ( MonadIO m
  , MonadThrow m
  , MonadMask m
  , MonadLogger m
  , MonadReader Global m
  ) => m ()
updateLogged = do
  g <- ask
  let fi = g.fileinfo
  let editor = g.config.editor
  let tempFileTemplate = fi.updateFilename <.> fi.updateFileExt
  withSystemTempFile tempFileTemplate $ \f _ -> do
    mapPath <- liftIO $ getXdgDirectory XdgData fi.serialiseDataPath
    hasSerialisedData <- liftIO $ doesFileExist mapPath
    oldMap <- if hasSerialisedData then deserialiseMap mapPath
                                   else return mempty
    encodeFile' f oldMap
    liftIO $ editor.edit f
    decodeResult <- runReaderT (runExceptT $ decodeFile' f) g
    newMap <- case decodeResult of
                Left err -> do
                  logErrorN $ Text.unlines
                    ["Syntax error in parsing yaml:", err, "Failed, nothing changed"]
                  throwM (userError "Update fail, see log above")
                Right songMap -> return songMap
    let changes = oldMap `compareTo` newMap
    logInfoN $ if noChange changes then "Nothing new"
                                   else prettyPrintChanges changes
    manageSongsLogged changes
    liftIO $ serialiseMap newMap =<< getXdgDirectory XdgData fi.serialiseDataPath
