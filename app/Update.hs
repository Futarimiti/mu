{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards     #-}

module Update (update) where

import           Config                     (Config (..))
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Trans.Reader (ReaderT (runReaderT))
import qualified Data.Map                   as M
import           Data.Text                  (unpack)
import           Editor                     (Editor (..))
import           FileInfo                   (FileInfo (..))
import           Global                     (Global (..))
import           Lib
import           Prelude                    hiding (log)
import           System.Directory           (XdgDirectory (XdgData),
                                             doesFileExist, getXdgDirectory)
import           System.Exit                (exitFailure)
import           System.FilePath            ((<.>))
import           System.IO.Temp             (withSystemTempFile)
import           Update.Manage              (manageSongs)
import           Update.Parse               (decodeFile, encodeFile)

update :: Global -> IO ()
update g@(Global {..}) = do let fi = fileinfo
                            let tempFileTemplate = fi.updateFilename <.> fi.updateFileExt
                            withSystemTempFile tempFileTemplate $ \f _ -> do
                              mapPath <- getXdgDirectory XdgData fi.serialiseDataPath
                              hasSerialisedData <- doesFileExist mapPath
                              oldMap <- if hasSerialisedData then deserialiseMap mapPath
                                                             else return M.empty
                              encodeFile f oldMap
                              _ <- config.editor.edit f
                              e <- runExceptT $ decodeFile f
                              case e of
                                Left err -> do
                                  let msg = unpack err
                                  log $ unlines ["Syntax error in parsing yaml:", msg, "Failed, nothing changed"]
                                  exitFailure
                                Right newMap -> do
                                  let changes = oldMap `compareTo` newMap
                                  log (prettyPrintChanges changes)
                                  manage <- runReaderT manageSongs g
                                  manage changes
                                  serialiseMap newMap =<< getXdgDirectory XdgData fi.serialiseDataPath
