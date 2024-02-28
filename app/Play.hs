{-# LANGUAGE OverloadedRecordDot #-}

module Play (playSeq, shuffle, playSeqLogged, shuffleLogged) where

import           Config                 (Config (..))
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Logger   (LoggingT, logErrorN, logInfoN)
import           Control.Monad.Reader   (ReaderT, ask, asks, runReaderT,
                                         withReaderT)
import qualified Data.Text              as T
import           FileInfo               (FileInfo (audioFileExt))
import           Global
import           Lib
import           Messages               (Messages (songNotExist))
import           Player                 (Player (..))
import           Prelude                hiding (log)
import           System.Directory
import           System.FilePath
import           System.Random.Shuffle  (shuffleM)

playSeq :: Global -> [SongName] -> IO ()
playSeq g = runReaderT play1 g >>= mapM_

playSeqLogged :: MonadIO io => [SongName] -> ReaderT Global (LoggingT io) ()
playSeqLogged = mapM_ play1Logged

play1 :: Monad m => ReaderT Global m (SongName -> IO ())
play1 = do ext <- asks (audioFileExt . fileinfo)
           mdir <- asks (musicDir . config)
           player' <- asks (player . config)
           notExist <- asks (songNotExist . mess)
           return $ \song -> do let songFile = mdir </> song <.> ext
                                exists <- doesFileExist songFile
                                if exists then do log' ("-> " ++ song)
                                                  play player' songFile
                                          else do log (notExist song)

play1Logged :: MonadIO io => SongName -> ReaderT Global (LoggingT io) ()
play1Logged song = do fi <- asks fileinfo
                      config <- asks config
                      notExist <- asks (songNotExist . mess)
                      let songFile = config.musicDir </> song <.> fi.audioFileExt
                      exists <- liftIO $ doesFileExist songFile
                      if exists then do logInfoN ("-> " <> T.pack song)
                                        liftIO $ config.player.play songFile
                                else do logErrorN $ T.pack (notExist song)

shuffle :: MonadIO m => [SongName] -> ReaderT Global m ()
shuffle list = do g <- ask
                  config <- asks config
                  songs <- case list of
                             [] -> withReaderT fileinfo (songsIn config.musicDir)
                             ne -> return ne
                  shuffled <- liftIO $ shuffleM songs
                  liftIO $ playSeq g shuffled

-- if given, shuffle through specified songs, otherwise shuffle through all songs
shuffleLogged :: MonadIO io => [SongName] -> ReaderT Global (LoggingT io) ()
shuffleLogged list = do config <- asks config
                        songs <- case list of
                                   [] -> withReaderT fileinfo (songsIn config.musicDir)
                                   ne -> return ne
                        shuffled <- liftIO $ shuffleM songs
                        playSeqLogged shuffled
