module Config.Parse (parseFile) where

import           Config                 (Config)
import           Config.Parse.Spec      (parseSpec, specToConfig)
import           Control.Monad.IO.Class (MonadIO)

parseFile :: MonadIO io => FilePath -> io Config
parseFile f = specToConfig <$> parseSpec f

