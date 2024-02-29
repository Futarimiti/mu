{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards     #-}

module Update.Manage (manageSongsLogged) where

import           Config                 (Config (..))
import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Logger   (LoggingT, logInfoN)
import           Control.Monad.Reader   (ReaderT, asks)
import           Data.Map               (Map, traverseWithKey)
import           Downloader             (Downloader (download))
import           FileInfo               (FileInfo (..))
import           Global                 (Global (..))
import           Lib                    (Changes (..), SongName, URL)
import           Messages               (Messages (..))
import           System.Directory       (removeFile)
import           System.FilePath        ((<.>), (</>))

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

removeSong1Logged :: MonadIO io => SongName -> ReaderT Global (LoggingT io) ()
removeSong1Logged song = do mdir <- asks (musicDir . config)
                            ext <- asks (audioFileExt . fileinfo)
                            beginDel <- asks (beginDelete . mess)
                            let file = mdir </> song <.> ext
                            logInfoN $ beginDel file
                            liftIO $ removeFile file

downloadSong1Logged :: MonadIO io => SongName -> URL -> ReaderT Global (LoggingT io) ()
downloadSong1Logged song url = do mdir <- asks (musicDir . config)
                                  down <- asks (downloader . config)
                                  ext <- asks (audioFileExt . fileinfo)
                                  beginDown <- asks (beginDownload . mess)
                                  let file = mdir </> song <.> ext
                                  logInfoN $ beginDown song url
                                  liftIO $ down.download url file

reinstallSong1Logged :: MonadIO io => SongName -> URL -> ReaderT Global (LoggingT io) ()
reinstallSong1Logged song url = do removeSong1Logged song
                                   downloadSong1Logged song url
