{-# LANGUAGE DeriveAnyClass #-}

module Messages where

import           Data.Text    (Text, pack)
import           Dhall        (FromDhall, auto, input)
import           GHC.Generics (Generic)
import           Lib
import           Paths_mu     (getDataFileName)

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

messages :: IO Messages
messages = getDataFileName "messages.dhall" >>= input auto . pack
