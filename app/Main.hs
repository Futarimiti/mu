{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Config             (Config (..), Downloader (..), Editor (..),
                                     Player (..), defaultConfig, override)
import           Control.Monad      (guard, unless)
import           Data.Function      ((&))
import           Data.Functor       ((<&>))
import           Data.List          (isInfixOf, isSuffixOf)
import           Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NE ((!!))
import           Data.Map.Strict    (Map, difference, empty, findWithDefault,
                                     fromList, keys, toList)
import           Data.String        (IsString (fromString))
import           Data.Text          (append)
import           Dhall              (input, map, string)
import           System.Directory   (XdgDirectory (XdgCache), copyFile,
                                     doesDirectoryExist, doesFileExist,
                                     getXdgDirectory, listDirectory)
import           System.Environment (getArgs)
import           System.Exit        (die)
import           System.FilePath    (splitFileName, (</>))
import           System.IO          (readFile')
import           System.Process     (callProcess)
import           System.Random      (randomRIO)
import           Turtle             (touch)

main :: IO ()
main = getArgs >>= mu

-- user configurable parts and defaults (incomplete)
userConfig :: Config
userConfig = override undefined defaultConfig

mu :: [String]  -- commandline args
   -> IO ()
mu [] = shuffleAll
mu [x] = case x of
      "--update"  -> update
      "-u"        -> update
      "--upgrade" -> upgrade
      "-U"        -> upgrade
      ""          -> shuffleAll
      "-a"        -> shuffleAll
      "--all"     -> shuffleAll
      "--"        -> shuffleAll
      ('-': a)    -> die $ "unknown argument: -" ++ a ++ "\nshould you wish to perform a fuzzy search, run `mu -- -" ++ a ++ "`"
      track       -> playit track
mu ["--", name] = playit name
mu o            = die $ "unexpected arguments or combination: " ++ unwords o

-- call out editor on UPDATE_EDITINFO
-- user finish updating: check new tracks, removed tracks, tracks with updated url, etc
-- do corresponding actions: download new tracks, remove deleted tracks, re-download updated tracks
upgrade :: IO ()
upgrade = do e <- editor userConfig
             updateFile <- updateEditInfoFile
             updateFileExists <- doesFileExist updateFile
             unless updateFileExists createUpdateEditInfoFile
             edit e updateFile

             pastFile <- pastUpdateFile
             pastUpdateFileExists <- doesFileExist pastFile

             updatedRecord <- dhallInterpretRecord updateFile
             previousRecord <- if pastUpdateFileExists then dhallInterpretRecord pastFile else return dhallEmptyRecord
             doActions updatedRecord previousRecord
             makePastUpdateFile  -- last step

type URL = String
type Record = Map String URL

-- find the difference between new and old records:
-- new tracks
-- removed tracks
-- url changes
-- then do corresponding actions
doActions :: Record  -- new
          -> Record  -- old
          -> IO ()
doActions new old = do downloadTracks newTracks
                       removeTracks removedTracks
                       reinstallTracks urlChangedTracks
                         where newTracks = difference new old
                               removedTracks = difference old new
                               urlChangedTracks = toList new & filter (\(track, newUrl) -> getOldUrl track newUrl /= newUrl) & fromList
                                 where getOldUrl track newUrl = findWithDefault newUrl track old


downloadTracks :: Record -> IO ()
downloadTracks tracks = do putStrLn $ "new tracks: " ++ unwords (keys tracks)
                           mdir <- musicDir userConfig
                           d <- downloader userConfig
                           mapM_ (\(songname, url) -> download d url (mdir </> songname ++ ".mp3")) (toList tracks)

removeTracks :: Record -> IO ()
removeTracks tracks = do putStrLn $ "deleted tracks: " ++ unwords (keys tracks)
                         mdir <- musicDir userConfig
                         mapM_ (\songname -> rmi (mdir </> songname ++ ".mp3")) (keys tracks)
                           where rmi f = callProcess "rm" ["-i", "--", f]

reinstallTracks :: Record -> IO ()
reinstallTracks tracks = do putStrLn $ "reinstall tracks: " ++ unwords (keys tracks)
                            removeTracks tracks
                            downloadTracks tracks

dhallEmptyRecord :: Record
dhallEmptyRecord = empty

dhallInterpretRecord :: FilePath -> IO Record
dhallInterpretRecord f = do content <- readFile' f
                            input (Dhall.map string string) (append "toMap " (fromString content))

pastUpdateFile :: IO FilePath
pastUpdateFile = getXdgDirectory XdgCache "mu/LAST_UPDATE"

makePastUpdateFile :: IO ()
makePastUpdateFile = do p <- pastUpdateFile
                        u <- updateEditInfoFile
                        createFile p
                        copyFile u p

updateEditInfoFile :: IO FilePath
updateEditInfoFile = getXdgDirectory XdgCache "mu/UPDATE_EDITINFO"

createUpdateEditInfoFile :: IO ()
createUpdateEditInfoFile = do u <- updateEditInfoFile
                              createFile u
                              writeFile u updateEditInfoInitialContent

updateEditInfoInitialContent :: String
updateEditInfoInitialContent = [ "-- update your music library in this file."
                               , ""
                               , "-- to add a new track, put a new pair of strings: track name = url"
                               , "-- to remove a local track, delete or comment out corresponding entry"
                               , "-- to rename a local track or to update its source url, edit correspondent string"
                               , ""
                               , "{=}"
                               ] & unlines

-- no effect if given filepath already exist
createFile :: FilePath -> IO ()
createFile f = do let parentDir = fst . splitFileName $ f
                  mkdirp parentDir
                  directoryExists <- doesDirectoryExist parentDir
                  guard directoryExists
                  touch f
                    where mkdirp dir = callProcess "mkdir" ["-p", dir]

-- now the same as upgrade
update :: IO ()
update = upgrade

playit :: String -> IO ()
playit name = musicDir userConfig >>= flip randomFuzzy name >>= maybe (die $ "pattern matches no track: " ++ name) showNplay

  {- given destination directory and seed,
      perform a fuzzy search in the target directory
      and return result as a list of matched files.

      returns an empty list when:
      * no such directory
      * no pattern matches found
  -}
fuzzy :: FilePath  -- directory to be lookup for
      -> String  -- seed
      -> IO [FilePath]
fuzzy dir f = listAllMp3 dir <&> filter (f `isInfixOf`)

listAllMp3 :: FilePath -> IO [FilePath]
listAllMp3 dir = do dirExists <- doesDirectoryExist dir
                    if dirExists then listDirectory dir <&> filter (".mp3" `isSuffixOf`)
                                 else return []

-- randomly select one from an non-empty list
pickOne :: NonEmpty a -> IO a
pickOne xs = do n <- randomRIO (0, length xs - 1)
                return (xs NE.!! n)

-- fuzzy then pick one
randomFuzzy :: FilePath  -- target dir
            -> String  -- seed
            -> IO (Maybe FilePath)
randomFuzzy dir f = fuzzy dir f >>= mapM pickOne . nonEmpty
-- old method using shell command:
-- fuzzy dir f = readCreateProcess (shell ("set -o pipefail; find " ++ dir ++ " -maxdepth 1 -iname \"*" ++ f ++ "*.mp3\" | grep . | shuf -n 1")) ""

shuffleAll :: IO ()
-- shuffleAll = musicDir userConfig >>= shuffle (player userConfig)
shuffleAll = do mdir <- musicDir userConfig
                player' <- player userConfig
                shuffle player' mdir

-- given track name, play correspondent track in the musicDir
playMusic :: FilePath -> IO ()
-- playMusic track = musicDir userConfig >>= play player . (</> track)
playMusic track = do mdir <- musicDir userConfig
                     player' <- player userConfig
                     play player' (mdir </> track)

-- display track name then play it
showNplay :: FilePath -> IO ()
showNplay track = putStrLn ("-> " ++ green ++ track ++ nc) >> playMusic track
  where green = "\ESC[32m"
        nc = "\ESC[0"

