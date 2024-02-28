{-# LANGUAGE OverloadedRecordDot #-}

module Play (playSeqLogged, shuffleLogged) where

import           Config                (Config (..))
import           Control.Monad.Logger  (LoggingT, logErrorN, logInfoN)
import           Control.Monad.Reader  (MonadIO (..), ReaderT, asks,
                                        withReaderT)
import qualified Data.Text             as T
import           FileInfo              (FileInfo (..))
import           Global                (Global (..))
import           Lib                   (SongName, songsIn)
import           Messages              (Messages (..))
import           Player                (Player (..))
import           System.Directory      (doesFileExist)
import           System.FilePath       ((<.>), (</>))
import           System.Random.Shuffle (shuffleM)

playSeqLogged :: MonadIO io => [SongName] -> ReaderT Global (LoggingT io) ()
playSeqLogged = mapM_ play1Logged

play1Logged :: MonadIO io => SongName -> ReaderT Global (LoggingT io) ()
play1Logged song = do fi <- asks fileinfo
                      config <- asks config
                      notExist <- asks (songNotExist . mess)
                      let songFile = config.musicDir </> song <.> fi.audioFileExt
                      exists <- liftIO $ doesFileExist songFile
                      if exists then do logInfoN ("-> " <> T.pack song)
                                        liftIO $ config.player.play songFile
                                else do logErrorN $ T.pack (notExist song)

-- | If given, shuffle through specified songs, otherwise shuffle through all songs
shuffleLogged :: MonadIO io => [SongName] -> ReaderT Global (LoggingT io) ()
shuffleLogged list = do config <- asks config
                        songs <- case list of
                                   [] -> withReaderT fileinfo (songsIn config.musicDir)
                                   ne -> return ne
                        shuffled <- liftIO $ shuffleM songs
                        playSeqLogged shuffled
