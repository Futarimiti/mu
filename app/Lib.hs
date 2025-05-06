module Lib where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader   (MonadReader, asks)
import FileInfo               (FileInfo (..))
import Global                 (Global (..))
import System.Directory       (listDirectory)
import System.FilePath        (isExtensionOf, takeBaseName)

class HasAudioFileExt a where
  getAudioFileExt :: a -> String

instance HasAudioFileExt FileInfo where
  getAudioFileExt = audioFileExt

instance HasAudioFileExt Global where
  getAudioFileExt = audioFileExt . fileinfo

-- | Get basenames of audio files under a directory
songsIn :: (HasAudioFileExt a, MonadIO m, MonadReader a m)
        => FilePath -> m [String]
songsIn dir = do
  ext <- asks getAudioFileExt
  files <- liftIO $ listDirectory dir
  let audios = filter (ext `isExtensionOf`) files
  pure $ map takeBaseName audios
