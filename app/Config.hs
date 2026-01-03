module Config (Config, ConfigOf (..), RawConfig, getWith) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson              as Aeson
import Data.ByteString         (ByteString)
import Data.ByteString         qualified as BS
import Data.Default
import Data.Functor.Barbie
import Data.Proxy
import Data.String.Interpolate (i)
import Data.Typeable
import System.IO.Error         qualified as IO
import Types

getWith :: (MonadIO m, MonadLogger m, MonadCatch m) => Maybe FilePath -> m Config
getWith Nothing     = pure def
getWith (Just path) = resolve <$> fromFile path

fromFile
  :: (MonadIO m, MonadLogger m, MonadCatch m)
  => FilePath
  -> m RawConfig
fromFile path = do
  $logDebug [i|extracting config from config file #{path}|]
  decodeFileThrow @RawConfig path

decodeFileThrow
  :: forall a m. (Typeable a, MonadIO m, FromJSON a, MonadCatch m, MonadLogger m)
  => FilePath
  -> m a
decodeFileThrow path = do
  contents <- readFileBSThrow path
  $logDebug [i|#{path} size: #{BS.length contents} bytes|]
  out <- Aeson.throwDecodeStrict contents `catch` handle
  $logDebug [i|successfully decoded #{path}|]
  pure out
  where
    handle :: AesonException -> m a
    handle e = do
      let destType = typeRep (Proxy @a)
          AesonException msg = e
      $logError [i|failed to decode #{path} as #{destType}: #{msg}|]
      throwM e

readFileBSThrow
  :: forall m. (MonadLogger m, MonadIO m, MonadCatch m)
  => FilePath -> m ByteString
readFileBSThrow path = do
  $logDebug [i|trying to read #{path} as a file|]
  contents <- liftIO (BS.readFile path) `catchIOError` handle
  $logDebug [i|successfully read #{path}|]
  pure contents
  where
    handle e = do
      let msg = IO.ioeGetErrorString e
      $logError [i|failed to read #{path}: #{msg}|]
      throwM e

resolve :: RawConfig -> Config
resolve raw = bzipWith (\m d -> maybe d pure m) raw (def @Config)
