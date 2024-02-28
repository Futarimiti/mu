{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards     #-}

module Update.Manage (manageSongs, manageSongsLogged) where

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
import           Lib                    (Changes (..), SongName, URL, log)
import           Messages               (Messages (beginDelete, beginDownload, beginReinstall))
import           Prelude                hiding (log)
import           System.Directory       (removeFile)
import           System.FilePath        ((<.>), (</>))

-- | Download songs, remove songs, reinstall songs, etc.
manageSongs :: ReaderT Global IO (Changes SongName URL -> IO ())
manageSongs = do remove <- removeSongs
                 download <- downloadSongs
                 reinstall <- reinstallSongs
                 return $ \Changes {..} -> do remove deleted
                                              download new
                                              reinstall modified

manageSongsLogged :: MonadIO io => Changes SongName URL -> ReaderT Global (LoggingT io) ()
manageSongsLogged Changes {..} = do removeSongsLogged deleted
                                    downloadSongsLogged new
                                    reinstallSongsLogged modified

reinstallSongs :: ReaderT Global IO (Map SongName URL -> IO ())
reinstallSongs = do beginReinstall <- asks (beginReinstall . mess)
                    reinstall1 <- reinstallSong1
                    return $ \m -> void $ traverseWithKey (\song url -> do log $ beginReinstall song url
                                                                           reinstall1 song url) m

reinstallSongsLogged :: MonadIO io => Map SongName URL -> ReaderT Global (LoggingT io) ()
reinstallSongsLogged m = do beginReinstall <- asks (beginReinstall . mess)
                            void $ traverseWithKey (\song url -> do logInfoN $ T.pack $ beginReinstall song url
                                                                    reinstallSong1Logged song url) m

downloadSongs :: ReaderT Global IO (Map SongName URL -> IO ())
downloadSongs = do beginDL <- asks (beginDownload . mess)
                   download1 <- downloadSong1
                   return $ \m -> void $ traverseWithKey (\song url -> do log $ beginDL song url
                                                                          download1 song url) m

downloadSongsLogged :: MonadIO io => Map SongName URL -> ReaderT Global (LoggingT io) ()
downloadSongsLogged m = do beginDL <- asks (beginDownload . mess)
                           void $ traverseWithKey (\song url -> do logInfoN $ T.pack $ beginDL song url
                                                                   downloadSong1Logged song url) m

removeSongs :: ReaderT Global IO ([SongName] -> IO ())
removeSongs = do beginDel <- asks (beginDelete . mess)
                 remove1 <- removeSong1
                 return $ \l -> mapM_ (\song -> do log $ beginDel song
                                                   remove1 song) l

removeSongsLogged :: MonadIO io => [SongName] -> ReaderT Global (LoggingT io) ()
removeSongsLogged list = do beginDel <- asks (beginDelete . mess)
                            forM_ list $ \song -> do logInfoN $ T.pack $ beginDel song
                                                     removeSong1Logged song

removeSong1 :: ReaderT Global IO (SongName -> IO ())
removeSong1 = do mdir <- asks (musicDir . config)
                 ext <- asks (audioFileExt . fileinfo)
                 return $ \song -> removeFile (mdir </> song <.> ext)

removeSong1Logged :: MonadIO io => SongName -> ReaderT Global (LoggingT io) ()
removeSong1Logged song = do mdir <- asks (musicDir . config)
                            ext <- asks (audioFileExt . fileinfo)
                            logInfoN $ "Removing " <> T.pack song
                            liftIO $ removeFile (mdir </> song <.> ext)

downloadSong1 :: ReaderT Global IO (SongName -> URL -> IO ())
downloadSong1 = do mdir <- asks (musicDir . config)
                   down <- asks (downloader . config)
                   ext <- asks (audioFileExt . fileinfo)
                   return $ \song url -> down.download url (mdir </> song <.> ext)

downloadSong1Logged :: MonadIO io => SongName -> URL -> ReaderT Global (LoggingT io) ()
downloadSong1Logged song url = do mdir <- asks (musicDir . config)
                                  down <- asks (downloader . config)
                                  ext <- asks (audioFileExt . fileinfo)
                                  logInfoN $ "Downloading " <> T.pack song
                                  liftIO $ down.download url (mdir </> song <.> ext)

reinstallSong1 :: ReaderT Global IO (SongName -> URL -> IO ())
reinstallSong1 = do remove <- removeSong1
                    download <- downloadSong1
                    return $ \song url -> remove song >> download song url

reinstallSong1Logged :: MonadIO io => SongName -> URL -> ReaderT Global (LoggingT io) ()
reinstallSong1Logged song url = do removeSong1Logged song
                                   downloadSong1Logged song url
