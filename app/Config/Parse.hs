{-# LANGUAGE OverloadedStrings #-}

module Config.Parse (decodeFile, encodeFile) where

import           Control.Arrow              (ArrowChoice (left))
import           Control.Monad.Trans.Class  (MonadTrans (lift))
import           Control.Monad.Trans.Except (ExceptT (..))
import           Data.Map                   (Map)
import           Data.Text                  (pack, unpack)
import           Data.Yaml                  (decodeFileEither,
                                             prettyPrintParseException)
import qualified Data.Yaml                  as Y
import           FileInfo                   (FileInfo (..), fileinfo)
import           Lib
import           Messages                   (Messages (notSupportedUpdateFileFormat),
                                             messages)

-- | Decode a config format from a file into songname-url map
decodeFile :: FilePath -> ExceptT ErrorMessage IO (Map SongName URL)
decodeFile f = do fi <- lift fileinfo
                  let format = unpack $ updateFileFormat fi
                  case format of
                    "yaml" -> decodeYamlExceptT f
                    _ -> do m <- lift messages
                            ExceptT . return . Left $ notSupportedUpdateFileFormat m format

-- | Encode songname-url map in a human readable config format
-- and save to the given file
encodeFile :: FilePath  -- dest
           -> Map SongName URL
           -> IO ()
encodeFile f m = do fi <- fileinfo
                    let format = unpack $ updateFileFormat fi
                    case format of
                      "yaml" -> Y.encodeFile f m
                      _ -> do mess <- messages
                              error . unpack $ notSupportedUpdateFileFormat mess format

--- impl

decodeYamlExceptT :: FilePath -> ExceptT ErrorMessage IO (Map SongName URL)
decodeYamlExceptT f = ExceptT $ left (pack . prettyPrintParseException) <$> decodeFileEither f
