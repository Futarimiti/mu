{-# LANGUAGE DeriveAnyClass #-}

module FileInfo (FileInfo(..), parseFile) where

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Text              (pack)
import           Dhall                  (FromDhall, auto, input)
import           GHC.Generics           (Generic)

data FileInfo = FileInfo { updateFilename    :: FilePath
                         , updateFileExt     :: String
                         , updateFileFormat  :: String
                         , serialiseDataPath :: FilePath  -- NOTE: NEED TO PREPEND XDG_DATA_HOME
                         , audioFileExt      :: String
                         , configFilePath    :: FilePath  -- NOTE: NEED TO PREPEND XDG_CONFIG_HOME
                         } deriving (Generic, FromDhall)

parseFile :: MonadIO io => FilePath -> io FileInfo
parseFile = liftIO . input auto . pack
