{-# LANGUAGE OverloadedRecordDot #-}

module Update (update) where

import           Config                     (Config (..))
import           Control.Monad.Trans.Except (runExceptT)
import qualified Data.Map                   as M
import           Data.Text                  (unpack)
import           Editor                     (Editor (..))
import           FileInfo                   (FileInfo (..), fileinfo)
import           Lib
import           Prelude                    hiding (log)
import           System.Directory           (XdgDirectory (XdgData),
                                             doesFileExist, getXdgDirectory)
import           System.FilePath            ((<.>))
import           System.IO.Temp             (withSystemTempFile)
import           Update.Manage              (manageSongs)
import           Update.Parse               (decodeFile, encodeFile)

update :: Config -> IO ()
update c = do fi <- fileinfo
              let tempFileTemplate = fi.updateFilename <.> fi.updateFileExt
              withSystemTempFile tempFileTemplate $ \f _ -> do
                hasSerialisedData <- doesFileExist =<< getXdgDirectory XdgData fi.serialiseDataPath
                oldMap <- if hasSerialisedData then getXdgDirectory XdgData fi.serialiseDataPath >>= deserialiseMap
                                               else return M.empty

                encodeFile f oldMap
                c.editor.edit f
                e <- runExceptT $ decodeFile f
                case e of
                  Left err -> error (unpack err)
                  Right newMap -> do
                    let changes = oldMap `compareTo` newMap
                    log (prettyPrintChanges changes)
                    manageSongs c changes
                    serialiseMap newMap =<< getXdgDirectory XdgData fi.serialiseDataPath

