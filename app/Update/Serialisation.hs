module Update.Serialisation (deserialiseMap, serialiseMap) where

import           Codec.Serialise        (Serialise, readFileDeserialise,
                                         writeFileSerialise)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Map               (Map)
import           System.Directory       (createDirectoryIfMissing)
import           System.FilePath        (takeDirectory)

deserialiseMap :: MonadIO io => (Ord a, Serialise a, Serialise b) => FilePath -> io (Map a b)
deserialiseMap = liftIO <$> readFileDeserialise

serialiseMap :: MonadIO io => (Ord a, Serialise a, Serialise b) => Map a b -> FilePath -> io ()
serialiseMap m f = liftIO $ do
  createDirectoryIfMissing True (takeDirectory f)
  writeFileSerialise f m

