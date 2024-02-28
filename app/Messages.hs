{-# LANGUAGE DeriveAnyClass #-}

module Messages (Messages(..), parseFile) where

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Text              (Text, pack)
import           Dhall                  (FromDhall, auto, input)
import           GHC.Generics           (Generic)
import           Lib

data Messages = Messages { cannotInferDefaultPlayerByOS :: OS -> Text
                         , cannotInferMusicDir          :: Text
                         , mustManuallySetDownloader    :: Text
                         , notSupportedUpdateFileFormat :: String -> Text
                         , beginDownload                :: SongName -> URL -> String
                         , beginDelete                  :: SongName -> String
                         , beginReinstall               :: SongName -> URL -> String
                         , illegalEmptyCommand          :: String
                         , songNotExist                 :: SongName -> String
                         } deriving (Generic, FromDhall)

parseFile :: MonadIO io => FilePath -> io Messages
parseFile = liftIO . input auto . pack
