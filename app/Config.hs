module Config (Config(..), Player(..), defaultConfig, override, Editor(..), Downloader(..)) where

import           Control.Monad            (void)
import           Data.Bool                (bool)
import           Data.Functor             ((<&>))
import           Data.List                (singleton)
import           System.Environment       (getEnv)
import           System.Exit              (ExitCode (..))
import           System.FilePath          ((</>))
import           System.Process           (CreateProcess (..), callProcess,
                                           proc, readProcessWithExitCode, shell,
                                           waitForProcess, withCreateProcess)
import           System.Process.Internals (ProcessHandle)

defaultConfig :: Config
defaultConfig = Config { musicDir = musicDir'
                       , editor = shellEditor
                       -- , player = existsShellCommand "mpv" <&> bool mpv afplay . not
                       , player = return afplay
                       , downloader = existsShellCommand "mpv" <&> bool ytdlp (error "Fatal: no downloader given") . not
                       }

data Config = Config { musicDir   :: IO FilePath
                     , editor     :: IO Editor
                     , player     :: IO Player
                     , downloader :: IO Downloader
                     }

newtype Editor = Editor { edit :: FilePath -> IO () }

data Player = Player { play    :: FilePath -> IO ()
                     , shuffle :: FilePath -> IO ()
                     , quit    :: IO ()
                     }

type URL = String

-- downloader: able to download mp3 audio given target url as source and filepath of downloaded track
newtype Downloader = Downloader { download :: URL -> FilePath -> IO () }

shellEditor :: IO Editor
shellEditor = do e <- getEnv "EDITOR"
                 return $ Editor { edit = callProcess e . singleton }

-- eval "$downloader -x --audio-format mp3 '$link' -o $HOME/Music/$name.mp3"
-- verbose?
ytdlp :: Downloader
ytdlp = Downloader { download = \url dest -> callProcess "yt-dlp" ["-x", "--audio-format", "mp3", url, "-o", dest]
                   }

musicDir' :: IO FilePath
musicDir' = getEnv "HOME" <&> (</> "Music")

existsShellCommand :: String -> IO Bool
existsShellCommand cmd = readProcessWithExitCode "command" ["-v", "--", cmd] "" <&> \(b, _, _) -> b == ExitSuccess

-- mpv :: Player
-- mpv = Player { play = \track -> withCreateProcess (proc "mpv" ["--no-video", "--", track]) vwait
--              , shuffle = \mdir -> withCreateProcess (shell "mpv --no-video --shuffle -- mpv -- $(find . -maxdepth 1 -iname \"*.mp3\" | cut -d/ -f2-)") { cwd = Just mdir } vwait
--              }

-- note: afplay does not support --
afplay :: Player
afplay = afplayWithQuality True

-- high quality? True -> 1; False -> 0
afplayWithQuality :: Bool -> Player
afplayWithQuality b  = Player { play = \track -> withCreateProcess (proc "afplay" ["-q", q, track]) vwait
                              , shuffle = \mdir -> withCreateProcess (shell $ "for f in $(find . -maxdepth 1 -iname '*.mp3' -type f | shuf); do afplay -q " ++ q ++ " $f; done") { cwd = Just mdir } vwait
                              , quit = withCreateProcess (shell "while [[ $(pgrep afplay) ]]; do killall afplay > /dev/null 2>&1; done") vwait
                              }
                                where q = if b then "1" else "0"

vwait :: p1 -> p2 -> p3 -> ProcessHandle -> IO ()
vwait _ _ _ p = void $ waitForProcess p

-- override default config with given user config
-- no idea of how to implement as of now, so it always return default config
override :: Config  -- user
         -> Config  -- default
         -> Config
override _user def = def

