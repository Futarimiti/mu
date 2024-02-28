{-# LANGUAGE DeriveAnyClass #-}

module FileInfo (FileInfo(..), fileinfo, parseFile) where

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Text              (pack)
import           Dhall                  (FromDhall, auto, input)
import           GHC.Generics           (Generic)
import           Paths_mu               (getDataFileName)

data FileInfo = FileInfo { updateFilename    :: FilePath
                         , updateFileExt     :: String
                         , updateFileFormat  :: String
                         , serialiseDataPath :: FilePath  -- NOTE: NEED TO PREPEND XDG_DATA_HOME
                         , audioFileExt      :: String
                         , configFilePath    :: FilePath  -- NOTE: NEED TO PREPEND XDG_CONFIG_HOME
                         } deriving (Generic, FromDhall)

{-# DEPRECATED fileinfo "inject fileinfo from Global instead" #-}
fileinfo :: MonadIO io => io FileInfo
fileinfo = liftIO (getDataFileName "fileinfo.dhall" >>= input auto . pack)

parseFile :: MonadIO io => FilePath -> io FileInfo
parseFile = liftIO . input auto . pack
