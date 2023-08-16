{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards     #-}

module Update (update) where

import           Config                     (Config (..))
import           Config.Parse               (decodeFile)
import           Control.Monad              (void)
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Map                   (Map, traverseWithKey)
import           Data.Text                  (unpack)
import           Downloader                 (download)
import           Editor                     (Editor (..))
import           FileInfo                   (FileInfo (..), fileinfo)
import           Lib
import           Messages                   (Messages (..), messages)
import           Prelude                    hiding (log)
import           System.Directory           (XdgDirectory (XdgData),
                                             getXdgDirectory, removeFile)
import           System.FilePath            ((<.>), (</>))
import           System.IO.Temp             (withSystemTempFile)

update :: Config -> IO ()
update c = do fi <- fileinfo
              let tempFileTemplate = fi.updateFilename <.> fi.updateFileExt
              withSystemTempFile tempFileTemplate $ \f _ -> do
                c.editor.edit f
                e <- runExceptT $ decodeFile f
                case e of
                  Left err -> error (unpack err)
                  Right newMap -> do
                    oldMap :: Map SongName URL <- deserialiseMap fi.serialiseDataPath
                    let changes = oldMap `compareTo` newMap
                    log (prettyPrintChanges changes)
                    manageSongs c changes
                    serialiseMap newMap =<< getXdgDirectory XdgData fi.serialiseDataPath

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
                                       downloader.download (musicDir </> url <.> fi.audioFileExt) song

reinstallSong :: Config -> SongName -> URL -> IO ()
reinstallSong c song url = removeSong c song
                        >> downloadSong c song url
