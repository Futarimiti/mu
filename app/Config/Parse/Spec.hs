{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE RecordWildCards #-}

module Config.Parse.Spec (ConfigSpec(..), parseSpec, specToConfig) where

import           Config                 (Config (..))
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Dhall                  (FromDhall, auto, inputFile)
import           Downloader             (Downloader (Downloader))
import           Editor                 (Editor (Editor))
import           GHC.Generics           (Generic)
import           Lib
import           Player                 (Player (Player))
import           System.Process         (callProcess)

-- | Spec for user config
data ConfigSpec = Spec { editorCommand      :: FilePath -> Command
                       , musicPlayerCommand :: SongName -> Command
                       , downloaderCommand  :: URL -> FilePath -> Command
                       , audioFileStoreDir  :: FilePath
                       } deriving (Generic, FromDhall)

parseSpec :: MonadIO io => FilePath -> io ConfigSpec
parseSpec = parseDhallSpec

specToConfig :: ConfigSpec -> Config
specToConfig Spec {..} = Config { player = Player $ \song -> callProcess (head (musicPlayerCommand song)) (tail (musicPlayerCommand song))
                                , musicDir = audioFileStoreDir
                                , editor = Editor $ \f -> callProcess (head (editorCommand f)) (tail (editorCommand f))
                                , downloader = d
                                } where d = Downloader $ \url f -> let cmd = downloaderCommand url f
                                                                    in callProcess (head cmd) (tail cmd)


--- impl

parseDhallSpec :: MonadIO io => FilePath -> io ConfigSpec
parseDhallSpec = liftIO <$> inputFile auto

-- parseDhallSpec :: FilePath -> IO ConfigSpec
-- parseDhallSpec = parseWithDefaults

-- parseWithDefaults :: String -> IO ConfigSpec
-- parseWithDefaults f = do defaultsFile <- getDataFileName "default-spec.dhall"
--                          input auto (pack defaultsFile <> " // " <> pack f)

