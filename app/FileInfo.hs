{-# LANGUAGE DeriveAnyClass #-}

module FileInfo (FileInfo(..), fileinfo) where

import           Data.Text    (pack)
import           Dhall        (FromDhall, auto, input)
import           GHC.Generics (Generic)
import           Paths_mu     (getDataFileName)

data FileInfo = FileInfo { updateFilename    :: FilePath
                         , updateFileExt     :: String
                         , updateFileFormat  :: String
                         , serialiseDataPath :: FilePath  -- NOTE: NEED TO PREPEND XDG_DATA_HOME
                         , audioFileExt      :: String
                         , configFilePath    :: FilePath  -- NOTE: NEED TO PREPEND XDG_CONFIG_HOME
                         } deriving (Generic, FromDhall)

fileinfo :: IO FileInfo
fileinfo = getDataFileName "fileinfo.dhall" >>= input auto . pack
