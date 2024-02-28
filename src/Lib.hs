module Lib where

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Reader   (ReaderT, asks)
import           Data.Map               (Map)
import           Data.Text              (Text)
import           FileInfo               (FileInfo (..))
import           System.Directory       (listDirectory)
import           System.FilePath        (isExtensionOf, takeBaseName)

-- | Command and options/args
type Command = [String]
type SongName = String
type URL = FilePath
type OS = String
type ErrorMessage = Text

songsIn :: MonadIO m => FilePath -> ReaderT FileInfo m [String]  -- basenames
songsIn dir = do ext <- asks audioFileExt
                 files <- liftIO $ listDirectory dir
                 let audios = filter (ext `isExtensionOf`) files
                 return $ map takeBaseName audios

data Changes a b = Changes { new      :: Map a b
                           , modified :: Map a b
                           , deleted  :: [a]
                           } deriving (Show, Eq)

