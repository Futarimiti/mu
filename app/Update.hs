{-# LANGUAGE OverloadedRecordDot #-}

module Update (updateLogged) where

import           Config                 (Config (..))
import           Control.Monad.Catch    (MonadMask)
import           Control.Monad.Except   (runExceptT)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Logger   (LoggingT (..), logErrorN, logInfoN)
import           Control.Monad.Reader   (MonadReader (ask),
                                         ReaderT (runReaderT), asks)
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

updateLogged :: (MonadIO io, MonadMask io) => ReaderT Global (LoggingT io) ()
updateLogged = do fi <- asks fileinfo
                  g <- ask
                  editor <- asks (editor . config)
                  let tempFileTemplate = fi.updateFilename <.> fi.updateFileExt
                  withSystemTempFile tempFileTemplate $ \f _ -> do
                    mapPath <- liftIO $ getXdgDirectory XdgData fi.serialiseDataPath
                    hasSerialisedData <- liftIO $ doesFileExist mapPath
                    oldMap <- if hasSerialisedData then deserialiseMap mapPath else return mempty
                    encodeFile' f oldMap
                    liftIO $ editor.edit f
                    decodeResult <- runExceptT $ runReaderT (decodeFile' f) g
                    case decodeResult of
                      Left err -> logErrorN $ Text.unlines ["Syntax error in parsing yaml:", err, "Failed, nothing changed"]
                      Right newMap -> do
                        let changes = oldMap `compareTo` newMap
                        logInfoN $ if noChange changes then "Nothing new"
                                                       else prettyPrintChanges changes
                        manageSongsLogged changes
                        liftIO $ serialiseMap newMap =<< getXdgDirectory XdgData fi.serialiseDataPath


