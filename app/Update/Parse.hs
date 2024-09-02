{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module Update.Parse (decodeFile', encodeFile') where

import           Control.Arrow             (ArrowChoice (left))
import           Control.Monad.Except      (MonadError (..), liftEither)
import           Control.Monad.IO.Class    (MonadIO (liftIO))
import           Control.Monad.Reader      (MonadReader, asks)
import           Data.Map                  (Map)
import           Data.Text                 (pack, unpack)
import           Data.Yaml                 (decodeFileEither,
                                            prettyPrintParseException)
import qualified Data.Yaml                 as Yaml (encodeFile)
import           FileInfo                  (FileInfo (..))
import           Global                    (Global (..))
import           Types
import           Messages                  (Messages (..))

-- | Decode a config format from a file into songname-url map
decodeFile' :: (MonadIO m, MonadError ErrorMessage m, MonadReader Global m)
            => FilePath
            -> m (Map SongName URL)
decodeFile' f = do fi <- asks fileinfo
                   case fi.updateFileFormat of
                     "yaml" -> decodeYamlExceptT f
                     _ -> do m <- asks mess
                             throwError $ notSupportedUpdateFileFormat m fi.updateFileFormat

-- | Encode songname-url map in a human readable config format
-- and save to the given file
encodeFile' :: (MonadIO m, MonadReader Global m) => FilePath -> Map SongName URL -> m ()
encodeFile' f m = do fi <- asks fileinfo
                     case fi.updateFileFormat of
                       "yaml" -> liftIO $ Yaml.encodeFile f m
                       _ -> do mess_ <- asks mess
                               error . unpack $ notSupportedUpdateFileFormat mess_ fi.updateFileFormat

--- impl


decodeYamlExceptT :: (MonadIO m, MonadError ErrorMessage m)
                  => FilePath
                  -> m (Map SongName URL)
decodeYamlExceptT f = do
  either' <- fmap (left (pack . prettyPrintParseException)) (liftIO (decodeFileEither f))
  liftEither either'
