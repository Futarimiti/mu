{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
module Update (updateLogged) where

import           Config                 (Config (..))
import           Control.Monad          (unless)
import           Control.Monad.Catch    (MonadMask)
import           Control.Monad.Except   (runExceptT)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Logger   (LoggingT (..), logErrorN, logInfoN)
import           Control.Monad.Reader   (MonadReader (ask),
                                         ReaderT (runReaderT), asks)
import qualified Data.Map               as M
import qualified Data.Text              as T
import           Editor                 (Editor (..))
import           FileInfo               (FileInfo (..))
import           Global                 (Global (..))
import           Lib
import           Prelude                hiding (log)
import           System.Directory       (XdgDirectory (XdgData), doesFileExist,
                                         getXdgDirectory)
import           System.FilePath        ((<.>))
import           System.IO.Temp         (withSystemTempFile)
import           Update.Manage          (manageSongsLogged)
import           Update.Parse           (decodeFile', encodeFile')

updateLogged :: (MonadIO io, MonadMask io) => ReaderT Global (LoggingT io) ()
updateLogged = do fi <- asks fileinfo
                  g <- ask
                  editor <- asks (editor . config)
                  let tempFileTemplate = fi.updateFilename <.> fi.updateFileExt
                  withSystemTempFile tempFileTemplate $ \f _ -> do
                    mapPath <- liftIO $ getXdgDirectory XdgData fi.serialiseDataPath
                    hasSerialisedData <- liftIO $ doesFileExist mapPath
                    oldMap <- if hasSerialisedData then deserialiseMap mapPath else return M.empty
                    runReaderT (encodeFile' f oldMap) g
                    liftIO $ editor.edit f
                    decodeResult <- runExceptT $ runReaderT (decodeFile' f) g
                    case decodeResult of
                      Left err -> logErrorN $ T.unlines ["Syntax error in parsing yaml:", err, "Failed, nothing changed"]
                      Right newMap -> do
                        let changes = oldMap `compareTo` newMap
                        logInfoN $ if noChange changes then "Nothing new"
                                                       else T.pack $ prettyPrintChanges changes
                        manageSongsLogged changes
                        liftIO $ serialiseMap newMap =<< getXdgDirectory XdgData fi.serialiseDataPath
