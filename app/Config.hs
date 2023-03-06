module Config (Config(..), Player(..), defaultConfig, override) where

import           Control.Monad            (void)
import           Data.Functor             ((<&>))
import           GHC.IO.Exception         (ExitCode (..))
import           System.Environment       (getEnv)
import           System.FilePath          ((</>))
import           System.Process           (CreateProcess (..), proc,
                                           readProcessWithExitCode, shell,
                                           waitForProcess, withCreateProcess)
import           System.Process.Internals (ProcessHandle)

data Player = Player { play    :: FilePath -> IO ()
                     , shuffle :: FilePath -> IO ()
                     }

data Config = Config { musicDir :: IO FilePath
                     , srcSh    :: IO FilePath
                     , editor   :: IO FilePath
                     , player   :: IO Player
                     }

defaultConfig :: Config
defaultConfig = Config { musicDir = musicDir'
                       , srcSh = musicDir' <&> (</> "src.sh")
                       , editor = getEnv "EDITOR"
                       , player = do exists <- existsShellCommand "mpv"
                                     return $ if exists then mpv else afplay
                       }

musicDir' :: IO FilePath
musicDir' = getEnv "HOME" <&> (</> "Music")

existsShellCommand :: String -> IO Bool
existsShellCommand cmd = readProcessWithExitCode "command" ["-v", "--", cmd] "" <&> \(b, _, _) -> b == ExitSuccess

mpv :: Player
mpv = Player { play = \track -> withCreateProcess (proc "mpv" ["--no-video", "--", track]) vwait
             , shuffle = \mdir -> withCreateProcess (shell "mpv --no-video --shuffle -- mpv -- $(find . -maxdepth 1 -iname \"*.mp3\" | cut -d/ -f2-)") { cwd = Just mdir } vwait
             }

afplay :: Player
afplay = Player { play = \track -> withCreateProcess (proc "afplay" ["--", track]) vwait
                , shuffle = \mdir -> withCreateProcess (shell "afplay -- $(find . -maxdepth 1 -iname \"*.mp3\")") { cwd = Just mdir } vwait
                }

vwait :: p1 -> p2 -> p3 -> ProcessHandle -> IO ()
vwait _ _ _ p = void $ waitForProcess p

-- override default config with given user config
-- no idea of how to implement as of now, so it always return default config
override :: Config  -- user
         -> Config  -- default
         -> Config
override _user def = def

