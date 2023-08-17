{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards     #-}

module Update.Manage (manageSongs) where
import           Config           (Config (..))
import           Control.Monad    (void)
import           Data.Map         (Map, traverseWithKey)
import           Downloader       (Downloader (download))
import           FileInfo         (FileInfo (..), fileinfo)
import           Lib              (Changes (..), SongName, URL, log)
import           Messages         (Messages (beginDelete, beginDownload, beginReinstall),
                                   messages)
import           Prelude          hiding (log)
import           System.Directory (removeFile)
import           System.FilePath  ((<.>), (</>))

-- | Download songs, remove songs, reinstall songs, etc.
manageSongs :: Config -> Changes SongName URL -> IO ()
manageSongs c Changes {..} = do removeSongs c deleted
                                downloadSongs c new
                                reinstallSongs c modified

reinstallSongs :: Config -> Map SongName URL -> IO ()
reinstallSongs c m = do mess <- messages
                        void $ traverseWithKey (\song url -> do log $ mess.beginReinstall song url
                                                                reinstallSong c song url) m

downloadSongs :: Config -> Map SongName URL -> IO ()
downloadSongs c m = do mess <- messages
                       void $ traverseWithKey (\song url -> do log $ mess.beginDownload song url
                                                               downloadSong c song url) m

removeSongs :: Config -> [SongName] -> IO ()
removeSongs c = mapM_ (\song -> do mess <- messages
                                   log $ mess.beginDelete song
                                   removeSong c song)

removeSong :: Config -> SongName -> IO ()
removeSong Config {..} song = do fi <- fileinfo
                                 removeFile (musicDir </> song <.> fi.audioFileExt)

downloadSong :: Config -> SongName -> URL -> IO ()
downloadSong Config {..} song url = do fi <- fileinfo
                                       downloader.download url (musicDir </> song <.> fi.audioFileExt)

reinstallSong :: Config -> SongName -> URL -> IO ()
reinstallSong c song url = removeSong c song
                        >> downloadSong c song url
