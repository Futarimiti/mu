{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards     #-}

module Update.Manage (manageSongsLogged) where

import           Config                 (Config (..))
import           Control.Exception      (IOException, try)
import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Logger   (MonadLogger, logErrorN, logInfoN,
                                         logWarnN)
import           Control.Monad.Reader   (MonadReader, asks)
import           Data.Map               (Map, traverseWithKey)
import qualified Data.Text              as Text
import           Downloader             (Downloader (download))
import           FileInfo               (FileInfo (..))
import           Global                 (Global (..))
import           Types
import           Messages               (Messages (..))
import           System.Directory       (removeFile)
import           System.FilePath        ((<.>), (</>))
import           System.IO.Error        (isDoesNotExistError)
import           Update.Changes         (Changes (..))

-- | Download songs, remove songs, reinstall songs, etc.
manageSongsLogged :: (MonadIO m, MonadLogger m, MonadReader Global m)
                  => Changes SongName URL
                  -> m ()
manageSongsLogged Changes {..} = do removeSongsLogged deleted
                                    downloadSongsLogged new
                                    reinstallSongsLogged modified

reinstallSongsLogged :: (MonadIO m, MonadLogger m, MonadReader Global m)
                     => Map SongName URL
                     -> m ()
reinstallSongsLogged = void . traverseWithKey reinstallSong1Logged

downloadSongsLogged :: (MonadIO m, MonadLogger m, MonadReader Global m)
                    => Map SongName URL
                    -> m ()
downloadSongsLogged = void . traverseWithKey downloadSong1Logged

removeSongsLogged :: (MonadIO m, MonadLogger m, MonadReader Global m)
                  => [SongName]
                  -> m ()
removeSongsLogged = mapM_ $ \song -> do
  res <- removeSong1Logged song
  case res of
    Left e
      | isDoesNotExistError e -> do
        logWarnN $ "Song " <> Text.pack song <> " not found"
        logWarnN "Should I move on as if nothing happened? [y/n]"
        response <- liftIO getLine
        case response of
          "y" -> return ()
          _   -> logErrorN "Aborted"
      | otherwise -> logErrorN $ "Error removing "
                                  <> Text.pack song
                                  <> ": "
                                  <> Text.pack (show e)
    _                        -> return ()

removeSong1Logged :: (MonadIO m, MonadLogger m, MonadReader Global m)
                  => SongName
                  -> m (Either IOException ())
removeSong1Logged song = do file <- getSongPath song
                            beginDel <- asks (beginDelete . mess)
                            logInfoN $ beginDel file
                            liftIO . try $ removeFile file

downloadSong1Logged :: (MonadIO m, MonadLogger m, MonadReader Global m)
                    => SongName
                    -> URL
                    -> m (Either IOException ())
downloadSong1Logged song url = do down <- asks (downloader . config)
                                  beginDown <- asks (beginDownload . mess)
                                  file <- getSongPath song
                                  logInfoN $ beginDown song url
                                  liftIO . try $ down.download url file

getSongPath :: MonadReader Global m => SongName -> m FilePath
getSongPath song = do mdir <- asks (musicDir . config)
                      ext <- asks (audioFileExt . fileinfo)
                      return $ mdir </> song <.> ext

reinstallSong1Logged :: (MonadIO m, MonadLogger m, MonadReader Global m)
                     => SongName
                     -> URL
                     -> m (Either IOException ())
reinstallSong1Logged song url = removeSong1Logged song >> downloadSong1Logged song url
