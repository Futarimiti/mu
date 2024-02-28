{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards     #-}

module Update.Manage (manageSongsLogged) where

import           Config                 (Config (..))
import           Control.Monad          (forM_, void)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Logger   (LoggingT, logInfoN)
import           Control.Monad.Reader   (ReaderT, asks)
import           Data.Map               (Map, traverseWithKey)
import qualified Data.Text              as T
import           Downloader             (Downloader (download))
import           FileInfo               (FileInfo (..))
import           Global                 (Global (..))
import           Lib                    (Changes (..), SongName, URL)
import           Messages               (Messages (beginDelete, beginDownload, beginReinstall))
import           System.Directory       (removeFile)
import           System.FilePath        ((<.>), (</>))

-- | Download songs, remove songs, reinstall songs, etc.
manageSongsLogged :: MonadIO io => Changes SongName URL -> ReaderT Global (LoggingT io) ()
manageSongsLogged Changes {..} = do removeSongsLogged deleted
                                    downloadSongsLogged new
                                    reinstallSongsLogged modified

reinstallSongsLogged :: MonadIO io => Map SongName URL -> ReaderT Global (LoggingT io) ()
reinstallSongsLogged m = do beginReinstall <- asks (beginReinstall . mess)
                            void $ traverseWithKey (\song url -> do logInfoN $ T.pack $ beginReinstall song url
                                                                    reinstallSong1Logged song url) m

downloadSongsLogged :: MonadIO io => Map SongName URL -> ReaderT Global (LoggingT io) ()
downloadSongsLogged m = do beginDL <- asks (beginDownload . mess)
                           void $ traverseWithKey (\song url -> do logInfoN $ T.pack $ beginDL song url
                                                                   downloadSong1Logged song url) m

removeSongsLogged :: MonadIO io => [SongName] -> ReaderT Global (LoggingT io) ()
removeSongsLogged list = do beginDel <- asks (beginDelete . mess)
                            forM_ list $ \song -> do logInfoN $ T.pack $ beginDel song
                                                     removeSong1Logged song

removeSong1Logged :: MonadIO io => SongName -> ReaderT Global (LoggingT io) ()
removeSong1Logged song = do mdir <- asks (musicDir . config)
                            ext <- asks (audioFileExt . fileinfo)
                            logInfoN $ "Removing " <> T.pack song
                            liftIO $ removeFile (mdir </> song <.> ext)

downloadSong1Logged :: MonadIO io => SongName -> URL -> ReaderT Global (LoggingT io) ()
downloadSong1Logged song url = do mdir <- asks (musicDir . config)
                                  down <- asks (downloader . config)
                                  ext <- asks (audioFileExt . fileinfo)
                                  logInfoN $ "Downloading " <> T.pack song
                                  liftIO $ down.download url (mdir </> song <.> ext)

reinstallSong1Logged :: MonadIO io => SongName -> URL -> ReaderT Global (LoggingT io) ()
reinstallSong1Logged song url = do removeSong1Logged song
                                   downloadSong1Logged song url
