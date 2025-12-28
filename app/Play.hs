{-# LANGUAGE OverloadedStrings #-}

module Play (playSeqLogged, shuffleLogged) where

import Config                (Config (..))
import Control.Monad.Logger
import Control.Monad.Reader  (MonadIO (..), MonadReader, asks)
import FileInfo              (FileInfo (..))
import Global                (Global (..))
import Lib                   (songsIn)
import Messages              (Messages (..))
import Player                (Player (..))
import System.Directory      (doesFileExist)
import System.FilePath       ((<.>), (</>))
import System.Random.Shuffle (shuffleM)
import Types

playSeqLogged :: (MonadIO m, MonadLogger m, MonadReader Global m) => [SongName] -> m ()
playSeqLogged = mapM_ play1Logged

play1Logged :: (MonadIO m, MonadLogger m, MonadReader Global m) => SongName -> m ()
play1Logged song = do
  fi <- asks fileinfo
  config <- asks config
  notExist <- asks (songNotExist . mess)
  let songFile = config.musicDir </> song <.> fi.audioFileExt
  exists <- liftIO $ doesFileExist songFile
  if exists then do
    logCurrent song
    liftIO $ config.player.play songFile
  else logErrorN (notExist song)

-- | Log the current playing song
logCurrent :: (MonadReader Global m, MonadLogger m) => SongName -> m ()
logCurrent song = do
  currentPlay <- asks (currentPlaying . mess)
  logOtherN (LevelOther "playing") (currentPlay song)

-- | If given, shuffle through specified songs, otherwise shuffle through all songs
shuffleLogged :: (MonadIO m, MonadLogger m, MonadReader Global m) => [SongName] -> m ()
shuffleLogged list = do
  config <- asks config
  songs <- case list of
             [] -> songsIn config.musicDir
             ne -> pure ne
  shuffled <- liftIO $ shuffleM songs
  playSeqLogged shuffled
