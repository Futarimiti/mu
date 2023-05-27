{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Config              (Config (..), Downloader (..), Editor (..),
                                      Player (..), defaultConfig, override)
import           Control.Monad       (forM_, guard, unless, when)
import           Data.Function       ((&))
import           Data.Functor        ((<&>))
import           Data.List           (intercalate, isInfixOf)
import           Data.List.NonEmpty  (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty  as NE ((!!))
import           Data.Map.Strict     (Map, difference, empty, findWithDefault,
                                      fromList, keys, toList)
import           Data.Yaml           (Value)
import qualified Data.Yaml           as Yaml
import           Data.Yaml.Aeson     (Value (..))
import           System.Console.ANSI (Color (Green), ColorIntensity (Dull),
                                      ConsoleLayer (Foreground),
                                      SGR (Reset, SetColor), setSGR)
import           System.Directory    (XdgDirectory (XdgCache), copyFile,
                                      doesDirectoryExist, doesFileExist,
                                      getXdgDirectory, listDirectory)
import           System.Environment  (getArgs)
import           System.Exit         (die)
import           System.FilePath     (isExtensionOf, splitFileName, (<.>),
                                      (</>))
import           System.Process      (callProcess)
import           System.Random       (randomRIO)
import           Turtle              (rm, touch)

main :: IO ()
main = getArgs >>= mu

-- user configurable parts and defaults (incomplete)
userConfig :: Config
userConfig = override undefined defaultConfig

mu :: [String] -> IO ()
mu []             = shuffleAll
-- if first arg begins with -, read as option
mu (o@('-':_):as) = muReadOptionArgs o as
mu tracks         = playseq tracks

type Option = String  -- always starting with -
type Args = [String]
muReadOptionArgs :: Option -> Args -> IO ()
muReadOptionArgs "--update"  _ = update
muReadOptionArgs "-u"        _ = update
muReadOptionArgs "--upgrade" _ = upgrade
muReadOptionArgs "-U"        _ = upgrade
muReadOptionArgs "-a"        _ = shuffleAll
muReadOptionArgs "--all"     _ = shuffleAll
muReadOptionArgs "-q"        _ = quitPlayer
muReadOptionArgs "--"       [] = shuffleAll
muReadOptionArgs "--"       xs = playseq xs
muReadOptionArgs o           _ = die . concat $ [ "unrecognised option: ", o
                                                , "\n"
                                                , "should you wish to perform a fuzzy search for tracks with hyphen,"
                                                , "run `mu -- ", o, "`"
                                                ]

type TrackName = String
playseq :: [TrackName] -> IO ()
playseq = mapM_ playit

playit :: TrackName -> IO ()
playit name = musicDir userConfig
          >>= flip randomFuzzy name
          >>= maybe (die $ "pattern matches no track: " ++ name) showNplay

quitPlayer :: IO ()
quitPlayer = do p <- player userConfig
                quit p

-- call out editor on UPDATE_EDITINFO
-- user finish updating: check new tracks, removed tracks, tracks with updated url, etc
-- do corresponding actions: download new tracks, remove deleted tracks, re-download updated tracks
upgrade :: IO ()
upgrade = do e <- editor userConfig
             updateFile <- updateEditInfoFile
             updateFileExists <- doesFileExist updateFile
             unless updateFileExists createUpdateEditInfoFile
             edit e updateFile

             pastFile <- pastUpdateFilePath
             pastUpdateFileExists <- doesFileExist pastFile

             updatedRecord <- interpretDict updateFile
             previousRecord <- if pastUpdateFileExists then interpretDict pastFile else return emptyDict
             doActions updatedRecord previousRecord
             makePastUpdateFile  -- last step

type URL = String
type Record = Map String URL

interpretDict :: FilePath -> IO Record
interpretDict = yamlInterpretDict

yamlInterpretDict :: FilePath -> IO Record
yamlInterpretDict f = do primitive <- Yaml.decodeFileThrow f :: IO Value
                         case primitive of
                           Null -> return empty
                           _    -> Yaml.decodeFileThrow f

-- find the difference between new and old records:
-- new tracks
-- removed tracks
-- url changes
-- then do corresponding actions
doActions :: Record  -- new
          -> Record  -- old
          -> IO ()
doActions new old = do downloadTracks Verbose newTracks
                       removeTracks Verbose removedTracks
                       reinstallTracks Verbose urlChangedTracks
                         where newTracks = difference new old
                               removedTracks = difference old new
                               urlChangedTracks = toList new & filter (\(track, newUrl) -> getOldUrl track newUrl /= newUrl) & fromList
                                 where getOldUrl track newUrl = findWithDefault newUrl track old

data Verbose = Verbose | Silent
  deriving (Eq, Show)

printTracks :: Map String a -> Maybe String -> [Char] -> IO ()
printTracks tracks ok new = case keys tracks of
                               [] -> forM_ ok putStrLn
                               ks -> do setSGR [SetColor Foreground Dull Green]
                                        putStr new
                                        setSGR [Reset]
                                        putStr ('[' : intercalate ", " ks)
                                        putStrLn "]"

downloadTracks :: Verbose -> Record -> IO ()
downloadTracks v tracks = do when verbose printNewTracks
                             mdir <- musicDir userConfig
                             d <- downloader userConfig
                             mapM_ (\(songname, url) -> download d url (mdir </> songname <.> "mp3")) (toList tracks)
                               where verbose = v == Verbose
                                     printNewTracks = printTracks tracks (Just "all tracks up to date") "new tracks: "

removeTracks :: Verbose -> Record -> IO ()
removeTracks v tracks = do when verbose printRemovingTracks
                           mdir <- musicDir userConfig
                           mapM_ (\songname -> rm (mdir </> songname <.> ".mp3")) (keys tracks)
                             where verbose = v == Verbose
                                   printRemovingTracks = printTracks tracks (Just "no tracks to be removed") "tracks to be removed: "

reinstallTracks :: Verbose -> Record -> IO ()
reinstallTracks v tracks = do when verbose printReinstallingTracks
                              removeTracks Silent tracks
                              downloadTracks Silent tracks
                                where verbose = v == Verbose
                                      printReinstallingTracks = printTracks tracks Nothing "tracks to be reinstalled: "

emptyDict :: Record
emptyDict = empty

pastUpdateFilePath :: IO FilePath
pastUpdateFilePath = getXdgDirectory XdgCache "mu/LAST_UPDATE"

makePastUpdateFile :: IO ()
makePastUpdateFile = do p <- pastUpdateFilePath
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
updateEditInfoInitialContent = [ "# vim: ft=yaml"
                               , "# update your music library in this file."
                               , ""
                               , "# to add a new track, put a new pair of strings like `track name: url`"
                               , "# to remove a local track, delete or comment out corresponding entry"
                               , "# to rename a local track or to update source url, edit correspondent string"
                               , ""
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
                    if dirExists then listDirectory dir <&> filter ("mp3" `isExtensionOf`)
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
showNplay track = do setSGR [SetColor Foreground Dull Green]
                     putStr "-> "
                     setSGR [Reset]
                     putStrLn track
                     playMusic track

