{-# LANGUAGE DeriveAnyClass #-}

module FileInfo (FileInfo(..), fileinfo) where

import           Data.Text    (Text, pack)
import           Dhall        (FromDhall, auto, input)
import           GHC.Generics (Generic)
import           Paths_mu     (getDataFileName)

data FileInfo = FileInfo { updateFilename   :: Text
                         , updateFileExt    :: Text
                         , updateFileFormat :: Text
                         } deriving (Generic, FromDhall)

fileinfo :: IO FileInfo
fileinfo = getDataFileName "fileinfo.dhall" >>= input auto . pack
