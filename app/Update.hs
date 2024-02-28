{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards     #-}

module Update (updateLogged) where

import           Config                 (Config (..))
import           Control.Monad.Catch    (MonadMask)
import           Control.Monad.Except   (runExceptT)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Logger   (LoggingT (..), logErrorN, logInfoN)
import           Control.Monad.Reader   (MonadReader (ask),
                                         ReaderT (runReaderT), asks)
import           Data.List              (intercalate)
import           Data.Map               (Map, filterWithKey, intersection, keys,
                                         (!), (\\))
import           Data.Maybe             (catMaybes)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Editor                 (Editor (..))
import           FileInfo               (FileInfo (..))
import           Global                 (Global (..))
import           Lib                    (Changes (..), SongName, URL)
import           Prelude                hiding (log)
import           System.Directory       (XdgDirectory (XdgData), doesFileExist,
                                         getXdgDirectory)
import           System.FilePath        ((<.>))
import           System.IO.Temp         (withSystemTempFile)
import           Text.Printf            (printf)
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
                    runReaderT (encodeFile' f oldMap) g
                    liftIO $ editor.edit f
                    decodeResult <- runExceptT $ runReaderT (decodeFile' f) g
                    case decodeResult of
                      Left err -> logErrorN $ T.unlines ["Syntax error in parsing yaml:", err, "Failed, nothing changed"]
                      Right newMap -> do
                        let changes = oldMap `compareTo` newMap
                        logInfoN $ if noChange changes then "Nothing new"
                                                       else prettyPrintChanges changes
                        manageSongsLogged changes
                        liftIO $ serialiseMap newMap =<< getXdgDirectory XdgData fi.serialiseDataPath


noChange :: Changes a b -> Bool
noChange Changes {..} = null new && null modified && null deleted

compareTo :: (Ord a, Eq b)
          => Map a b  -- old
          -> Map a b  -- new
          -> Changes a b
compareTo old new = Changes added changed deleted
  where added = new \\ old
        changed = filterWithKey (\k v -> old ! k /= v) $ intersection new old
        deleted = keys $ old \\ new

prettyPrintChanges :: Changes SongName URL -> Text
prettyPrintChanges Changes {..} = T.unlines . catMaybes $ [newSongs, modifiedSongs, deletedSongs]
  where newSongs, modifiedSongs, deletedSongs :: Maybe Text
        newSongs = template "new songs" (keys new)
        modifiedSongs = template "to be reinstalled" (keys modified)
        deletedSongs = template "to be deleted" deleted
        template :: Text -> [SongName] -> Maybe Text
        template _ [] = Nothing
        template prompt items = Just $ T.pack $ printf "%s: [%s]" prompt (intercalate ", " items)

