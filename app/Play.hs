{-# LANGUAGE RecordWildCards #-}

module Play (playSeq, shuffle) where

import           Config                (Config (..))
import           FileInfo              (FileInfo (..), fileinfo)
import           Lib
import           Messages              (Messages (..), messages)
import           Player                (Player (play))
import           Prelude               hiding (log)
import           System.Directory      (doesFileExist)
import           System.FilePath       ((<.>), (</>))
import           System.Random.Shuffle (shuffleM)

playSeq :: Config -> [SongName] -> IO ()
playSeq = mapM_ . play1

-- | Note: "song name" is the basename of an audio file
-- which should not contain parent dirs or ext
play1 :: Config -> SongName -> IO ()
play1 Config {..} song = do songFileExt <- audioFileExt <$> fileinfo
                            let songFile = musicDir </> song <.> songFileExt
                            exists <- doesFileExist songFile
                            if exists then do log ("-> " ++ song)
                                              play player songFile
                                      else do mess <- messages
                                              log (songNotExist mess song)

shuffle :: Config -> IO ()
shuffle c@(Config {..}) = do shuffledAudioBasenames <- shuffleM =<< songsIn musicDir
                             playSeq c shuffledAudioBasenames
