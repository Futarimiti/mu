module Config.Parse (parseFile) where

import           Config            (Config)
import           Config.Parse.Spec (parseSpec, specToConfig)

parseFile :: FilePath -> IO Config
parseFile f = specToConfig <$> parseSpec f

