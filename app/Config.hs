module Config (Config(..), Player(..), defaultConfig, override) where

import           Control.Monad            (void)
import           Data.Bool                (bool)
import           Data.Functor             ((<&>))
import           System.Environment       (getEnv)
import           System.Exit              (ExitCode (..))
import           System.FilePath          ((</>))
import           System.Process           (CreateProcess (..), callProcess,
                                           proc, readProcessWithExitCode, shell,
                                           waitForProcess, withCreateProcess)
import           System.Process.Internals (ProcessHandle)

defaultConfig :: Config
defaultConfig = Config { musicDir = musicDir'
                       , srcSh = musicDir' <&> (</> "src.sh")
                       , editor = getEnv "EDITOR"
                       , player = existsShellCommand "mpv" <&> bool mpv afplay . not
                       , downloader = existsShellCommand "mpv" <&> bool ytdlp (error "Fatal: no downloader given") . not
                       }

data Config = Config { musicDir   :: IO FilePath
                     , srcSh      :: IO FilePath
                     , editor     :: IO FilePath
                     , player     :: IO Player
                     , downloader :: IO Downloader
                     }

data Player = Player { play    :: FilePath -> IO ()
                     , shuffle :: FilePath -> IO ()
                     }

type URL = String

-- downloader: able to download mp3 audio given target url as source and filepath of downloaded track
newtype Downloader = Downloader { download :: URL -> FilePath -> IO () }

-- eval "$downloader -x --audio-format mp3 '$link' -o $HOME/Music/$name.mp3"
-- verbose?
ytdlp :: Downloader
ytdlp = Downloader { download = \url dest -> callProcess "ytdlp" ["-x", "--audio-format", "mp3", url, "-o", dest]
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

