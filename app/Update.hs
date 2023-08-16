{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards     #-}

module Update (update) where

import           Config                     (Config (..))
import           Config.Parse               (decodeFile)
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Map                   (Map)
import           Data.Text                  (unpack)
import           Editor                     (Editor (..))
import           FileInfo                   (FileInfo (..), fileinfo)
import           Lib
import           Prelude                    hiding (log)
import           System.FilePath            ((<.>))
import           System.IO.Temp             (withSystemTempFile)

update :: Config -> IO ()
update c = do fi <- fileinfo
              let tempFileTemplate = unpack fi.updateFilename <.> unpack fi.updateFileExt
              withSystemTempFile tempFileTemplate (\f _ -> do
                c.editor.edit f
                e <- runExceptT $ decodeFile f
                case e of
                  Left err -> error (unpack err)
                  Right newMap -> do
                    oldMap :: Map SongName URL <- deserialiseMap fi.serialiseDataPath
                    let changes = oldMap `compareTo` newMap
                    manageSongs c changes
                    serialiseMap newMap fi.serialiseDataPath)

-- | Download songs, remove songs, reinstall songs, etc.
manageSongs :: Config -> Changes SongName URL -> IO ()
manageSongs c Changes {..} = do removeSongs c deleted
                                downloadSongs c new
                                reinstallSongs c modified

reinstallSongs :: Config -> Map SongName URL -> IO ()
reinstallSongs = undefined

downloadSongs :: Config -> Map SongName URL -> IO ()
downloadSongs = undefined

removeSongs :: Config -> [SongName] -> IO ()
removeSongs = undefined

