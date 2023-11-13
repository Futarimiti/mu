{-# LANGUAGE LambdaCase #-}

module Play (playSeq, shuffle) where

import           Config                     (Config (..))
import           Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT,
                                             withReaderT)
import           FileInfo                   (FileInfo (audioFileExt))
import           Global
import           Lib
import           Messages                   (Messages (songNotExist))
import           Player                     (Player (..))
import           Prelude                    hiding (log)
import           System.Directory
import           System.FilePath
import           System.Random.Shuffle      (shuffleM)

playSeq :: Global -> [SongName] -> IO ()
playSeq g = runReaderT play1 g >>= mapM_

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

shuffle :: Monad m => ReaderT Global m ([SongName] -> IO ())
shuffle = do mdir <- asks (musicDir . config)
             g <- ask
             songsIO <- ($ mdir) <$> withReaderT fileinfo songsIn
             return $ \case []    -> songsIO >>= shuffleM >>= playSeq g
                            songs -> shuffleM songs >>= playSeq g
