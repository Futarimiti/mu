module Lib where

import           Control.Monad              (guard)
import           Control.Monad.Trans.Class  (MonadTrans (..))
import           Control.Monad.Trans.Maybe  (MaybeT (..))
import           Control.Monad.Trans.Writer (WriterT (runWriterT))
import           Data.Bifunctor             (Bifunctor (second))
import           System.Directory           (doesDirectoryExist)
import           System.Environment         (lookupEnv)
import           System.FilePath            ((</>))
import           System.IO                  (hPutStrLn, stderr)

type SongName = String
type URL = FilePath
type OS = String

xdgMusicDirs :: [MaybeT IO FilePath]
xdgMusicDirs = [ do xdgMusic <- MaybeT $ lookupEnv "XDG_MUSIC_DIR"
                    exists <- lift $ doesDirectoryExist xdgMusic
                    guard exists
                    return xdgMusic
               , do home <- MaybeT (lookupEnv "HOME")
                    let muDir = home </> "Music"
                    exists <- lift $ doesDirectoryExist muDir
                    guard exists
                    return $ muDir </> "mu"
               ]

newtype DiffList a = DiffList { runDiffList :: [a] -> [a] }

instance Semigroup (DiffList a) where
  DiffList f <> DiffList g = DiffList (f . g)

instance Monoid (DiffList a) where
  mempty = DiffList id

instance Foldable DiffList where
  foldr :: (a -> b -> b) -> b -> DiffList a -> b
  foldr f x dl = foldr f x $ fromDiffList dl

toDiffList :: [a] -> DiffList a
toDiffList = DiffList . (++)

fromDiffList :: DiffList a -> [a]
fromDiffList = flip runDiffList []

type LogT m a = WriterT (DiffList String) m a

-- log :: Monad m => String -> LogT m ()
-- log m = tell (toDiffList [m])

log :: String -> IO ()
log = hPutStrLn stderr

runLogT :: Functor m => LogT m a -> m (a, [String])
runLogT m = second fromDiffList <$> runWriterT m
