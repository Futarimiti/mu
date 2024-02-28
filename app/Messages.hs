{-# LANGUAGE DeriveAnyClass #-}

module Messages (Messages(..), messages, parseFile) where

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Text              (Text, pack)
import           Dhall                  (FromDhall, auto, input)
import           GHC.Generics           (Generic)
import           Lib
import           Paths_mu               (getDataFileName)

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

{-# DEPRECATED messages "inject mess from Global instead" #-}
messages :: MonadIO io => io Messages
messages = liftIO (getDataFileName "messages.dhall" >>= input auto . pack)

parseFile :: MonadIO io => FilePath -> io Messages
parseFile = liftIO . input auto . pack
