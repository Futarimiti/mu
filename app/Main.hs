module Main (main) where

import           Config
import           Data.Functor       ((<&>))
import           Data.List          (isInfixOf, isSuffixOf)
import           Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NE ((!!))
import           System.Directory   (doesDirectoryExist, listDirectory)
import           System.Environment (getArgs)
import           System.Exit        (die)
import           System.FilePath    ((</>))
import           System.Process     (callProcess)
import           System.Random      (randomRIO)

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
      ('-': a)    -> die $ "unknown argument: -" ++ a
      track       -> playit track
mu ["--", name] = playit name
mu o            = die $ "unexpected arguments or combination: " ++ unwords o

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

-- source src.sh
update :: IO ()
update = srcSh userConfig >>= sourceScript

sourceScript :: FilePath -> IO ()
sourceScript = flip callProcess []

-- edit src.sh in editor, then source it
upgrade :: IO ()
upgrade = do ed <- editor userConfig
             srcsh <- srcSh userConfig
             callProcess ed [srcsh]
             update

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

