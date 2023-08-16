module Config.Parse (decodeFile, encodeFile) where

import           Control.Arrow              (ArrowChoice (left))
import           Control.Monad.Trans.Except (ExceptT (..))
import           Data.Map                   (Map)
import           Data.Yaml                  (decodeFileEither,
                                             prettyPrintParseException)
import qualified Data.Yaml                  as Y
import           Lib

-- | Decode a config format from a file into songname-url map
decodeFile :: FilePath -> ExceptT ErrorMessage IO (Map SongName URL)
decodeFile = decodeYamlExceptT

-- | Encode songname-url map in a human readable config format
-- and save to the given file
encodeFile :: FilePath  -- dest
           -> Map SongName URL
           -> IO ()
encodeFile = Y.encodeFile

--- impl

decodeYamlExceptT :: FilePath -> ExceptT ErrorMessage IO (Map SongName URL)
decodeYamlExceptT f = ExceptT (left prettyPrintParseException <$> decodeFileEither f)
