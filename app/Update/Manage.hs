{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards     #-}

module Update.Manage (manageSongs) where

import           Config                     (Config (..))
import           Control.Monad              (void)
import           Control.Monad.Trans.Reader (ReaderT, asks)
import           Data.Map                   (Map, traverseWithKey)
import           Downloader                 (Downloader (download))
import           FileInfo                   (FileInfo (..))
import           Global                     (Global (..))
import           Lib                        (Changes (..), SongName, URL, log)
import           Messages                   (Messages (beginDelete, beginDownload, beginReinstall))
import           Prelude                    hiding (log)
import           System.Directory           (removeFile)
import           System.FilePath            ((<.>), (</>))

-- | Download songs, remove songs, reinstall songs, etc.
manageSongs :: ReaderT Global IO (Changes SongName URL -> IO ())
manageSongs = do remove <- removeSongs
                 download <- downloadSongs
                 reinstall <- reinstallSongs
                 return $ \Changes {..} -> do remove deleted
                                              download new
                                              reinstall modified

reinstallSongs :: ReaderT Global IO (Map SongName URL -> IO ())
reinstallSongs = do beginReinstall <- asks (beginReinstall . mess)
                    reinstall1 <- reinstallSong1
                    return $ \m -> void $ traverseWithKey (\song url -> do log $ beginReinstall song url
                                                                           reinstall1 song url) m

downloadSongs :: ReaderT Global IO (Map SongName URL -> IO ())
downloadSongs = do beginDL <- asks (beginDownload . mess)
                   download1 <- downloadSong1
                   return $ \m -> void $ traverseWithKey (\song url -> do log $ beginDL song url
                                                                          download1 song url) m

removeSongs :: ReaderT Global IO ([SongName] -> IO ())
removeSongs = do beginDel <- asks (beginDelete . mess)
                 remove1 <- removeSong1
                 return $ \l -> mapM_ (\song -> do log $ beginDel song
                                                   remove1 song) l

removeSong1 :: ReaderT Global IO (SongName -> IO ())
removeSong1 = do mdir <- asks (musicDir . config)
                 ext <- asks (audioFileExt . fileinfo)
                 return $ \song -> removeFile (mdir </> song <.> ext)

downloadSong1 :: ReaderT Global IO (SongName -> URL -> IO ())
downloadSong1 = do mdir <- asks (musicDir . config)
                   down <- asks (downloader . config)
                   ext <- asks (audioFileExt . fileinfo)
                   return $ \song url -> down.download url (mdir </> song <.> ext)

reinstallSong1 :: ReaderT Global IO (SongName -> URL -> IO ())
reinstallSong1 = do remove <- removeSong1
                    download <- downloadSong1
                    return $ \song url -> remove song >> download song url
