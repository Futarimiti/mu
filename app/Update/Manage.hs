{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards     #-}

module Update.Manage (manageSongsLogged) where

import           Config                 (Config (..))
import           Control.Exception      (IOException, try)
import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Logger   (LoggingT, logInfoN)
import           Control.Monad.Reader   (MonadReader, ReaderT, asks)
import           Data.Map               (Map, traverseWithKey)
import           Downloader             (Downloader (download))
import           FileInfo               (FileInfo (..))
import           Global                 (Global (..))
import           Lib                    (SongName, URL)
import           Messages               (Messages (..))
import           System.Directory       (removeFile)
import           System.FilePath        ((<.>), (</>))
import           Update.Changes         (Changes (..))

-- | Download songs, remove songs, reinstall songs, etc.
manageSongsLogged :: MonadIO io => Changes SongName URL -> ReaderT Global (LoggingT io) ()
manageSongsLogged Changes {..} = do removeSongsLogged deleted
                                    downloadSongsLogged new
                                    reinstallSongsLogged modified

reinstallSongsLogged :: MonadIO io => Map SongName URL -> ReaderT Global (LoggingT io) ()
reinstallSongsLogged = void . traverseWithKey reinstallSong1Logged

downloadSongsLogged :: MonadIO io => Map SongName URL -> ReaderT Global (LoggingT io) ()
downloadSongsLogged = void . traverseWithKey downloadSong1Logged

removeSongsLogged :: MonadIO io => [SongName] -> ReaderT Global (LoggingT io) ()
removeSongsLogged = mapM_ removeSong1Logged

removeSong1Logged :: MonadIO io => SongName -> ReaderT Global (LoggingT io) (Either IOException ())
removeSong1Logged song = do file <- getSongPath song
                            beginDel <- asks (beginDelete . mess)
                            logInfoN $ beginDel file
                            liftIO . try $ removeFile file

downloadSong1Logged :: MonadIO io => SongName -> URL -> ReaderT Global (LoggingT io) (Either IOException ())
downloadSong1Logged song url = do down <- asks (downloader . config)
                                  beginDown <- asks (beginDownload . mess)
                                  file <- getSongPath song
                                  logInfoN $ beginDown song url
                                  liftIO . try $ down.download url file

getSongPath :: MonadReader Global m => SongName -> m FilePath
getSongPath song = do mdir <- asks (musicDir . config)
                      ext <- asks (audioFileExt . fileinfo)
                      return $ mdir </> song <.> ext

reinstallSong1Logged :: MonadIO io => SongName -> URL -> ReaderT Global (LoggingT io) (Either IOException ())
reinstallSong1Logged song url = removeSong1Logged song >> downloadSong1Logged song url
