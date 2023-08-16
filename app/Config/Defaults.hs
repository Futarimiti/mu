module Config.Defaults (defaults) where

import           Config                    (Config (..))
import           Control.Applicative       (asum)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (unpack)
import           Downloader                (Downloader (..))
import           Editor                    (Editor, notepad, shellEditor, vim)
import           Lib
import           Messages                  (Messages (..), messages)
import           Player                    (Player, afplayHQ)
import           System.Directory          (XdgDirectory (..), getXdgDirectory)
import           System.Info               (os)

defaultEditor :: IO Editor
defaultEditor = do shellEd <- runMaybeT shellEditor
                   return $ fromMaybe (case os of "mingw32" -> notepad
                                                  _         -> vim) shellEd

defaultPlayer :: IO Player
defaultPlayer = case os of "darwin" -> return afplayHQ
                           _        -> (\m -> error . unpack $ cannotInferDefaultPlayerByOS m os) <$> messages

defaultMusicDir :: IO FilePath
defaultMusicDir = do md <- runMaybeT $ asum xdgMusicDirs
                     maybe (getXdgDirectory XdgData "mu") return md

defaultDownloader :: IO Downloader
defaultDownloader = error . unpack . mustManuallySetDownloader <$> messages

defaults :: IO Config
defaults = do e <- defaultEditor
              p <- defaultPlayer
              m <- defaultMusicDir
              d <- defaultDownloader
              return Config { editor = e
                            , player = p
                            , downloader = d
                            , musicDir = m
                            }
