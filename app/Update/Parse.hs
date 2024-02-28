{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module Update.Parse (decodeFile, encodeFile, decodeFile', encodeFile') where

import           Control.Arrow             (ArrowChoice (left))
import           Control.Monad.Except      (ExceptT (..))
import           Control.Monad.IO.Class    (MonadIO (liftIO))
import           Control.Monad.Reader      (ReaderT, asks)
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Data.Map                  (Map)
import           Data.Text                 (pack, unpack)
import           Data.Yaml                 (decodeFileEither,
                                            prettyPrintParseException)
import qualified Data.Yaml                 as Y
import           FileInfo                  (FileInfo (..), fileinfo)
import           Global                    (Global)
import qualified Global                    as G
import           Lib
import           Messages                  (Messages (notSupportedUpdateFileFormat),
                                            messages)

-- | Decode a config format from a file into songname-url map
{-# DEPRECATED decodeFile "Use decodeFile' instead" #-}
decodeFile :: MonadIO io => FilePath -> ExceptT ErrorMessage io (Map SongName URL)
decodeFile f = do fi <- lift fileinfo  -- TODO: flawed, should be injected
                  let format = updateFileFormat fi
                  case format of
                    "yaml" -> decodeYamlExceptT f
                    _ -> do m <- lift messages  -- TODO: flawed, should be injected
                            ExceptT . return . Left $ notSupportedUpdateFileFormat m format

decodeFile' :: MonadIO io => FilePath -> ReaderT Global (ExceptT ErrorMessage io) (Map SongName URL)
decodeFile' f = do fi <- asks G.fileinfo
                   case fi.updateFileFormat of
                     "yaml" -> lift $ decodeYamlExceptT f
                     _ -> do m <- asks G.mess
                             lift . ExceptT . return . Left $ notSupportedUpdateFileFormat m fi.updateFileFormat

-- | Encode songname-url map in a human readable config format
-- and save to the given file
{-# DEPRECATED encodeFile "Use encodeFile' instead" #-}
encodeFile :: MonadIO io
           => FilePath  -- dest
           -> Map SongName URL
           -> io ()
encodeFile f m = do fi <- fileinfo  -- TODO: flawed, should be injected
                    let format = updateFileFormat fi
                    case format of
                      "yaml" -> liftIO $ Y.encodeFile f m
                      _ -> do mess <- messages  -- TODO: ditto
                              error . unpack $ notSupportedUpdateFileFormat mess format

encodeFile' :: MonadIO io => FilePath -> Map SongName URL -> ReaderT Global io ()
encodeFile' f m = do fi <- asks G.fileinfo
                     case fi.updateFileFormat of
                       "yaml" -> liftIO $ Y.encodeFile f m
                       _ -> do mess <- asks G.mess
                               error . unpack $ notSupportedUpdateFileFormat mess fi.updateFileFormat

--- impl

decodeYamlExceptT :: MonadIO io => FilePath -> ExceptT ErrorMessage io (Map SongName URL)
decodeYamlExceptT f = ExceptT $ left (pack . prettyPrintParseException) <$> liftIO (decodeFileEither f)
