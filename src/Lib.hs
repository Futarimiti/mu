{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE RecordWildCards     #-}

module Lib where

import           Codec.Serialise           (Serialise, readFileDeserialise,
                                            writeFileSerialise)
import           Control.Monad             (guard)
import           Control.Monad.IO.Class    (MonadIO (liftIO))
import           Control.Monad.Reader      (ReaderT, asks)
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Control.Monad.Writer      (WriterT (runWriterT))
import           Data.Bifunctor            (Bifunctor (second))
import           Data.List                 (intercalate)
import           Data.Map                  (Map, filterWithKey, intersection,
                                            keys, (!), (\\))
import           Data.Maybe                (catMaybes)
import           Data.Text                 (Text)
import           FileInfo                  (FileInfo (..))
import           System.Directory          (createDirectoryIfMissing,
                                            doesDirectoryExist, listDirectory)
import           System.Environment        (lookupEnv)
import           System.FilePath           (isExtensionOf, takeBaseName,
                                            takeDirectory, (</>))
import           System.IO                 (hPutStrLn, stderr)
import           Text.Printf               (printf)

-- | Command and options/args
type Command = [String]
type SongName = String
type URL = FilePath
type OS = String
type ErrorMessage = Text

deserialiseMap :: MonadIO io => (Ord a, Serialise a, Serialise b) => FilePath -> io (Map a b)
deserialiseMap = liftIO <$> readFileDeserialise

serialiseMap :: MonadIO io => (Ord a, Serialise a, Serialise b) => Map a b -> FilePath -> io ()
serialiseMap m f = liftIO $ do
  createDirectoryIfMissing True (takeDirectory f)
  writeFileSerialise f m

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
log m = hPutStrLn stderr ("[mu] " ++ m)

log' :: String -> IO ()
log' = hPutStrLn stderr

runLogT :: Functor m => LogT m a -> m (a, [String])
runLogT m = second fromDiffList <$> runWriterT m

data Changes a b = Changes { new      :: Map a b
                           , modified :: Map a b
                           , deleted  :: [a]
                           } deriving (Show, Eq)

compareTo :: (Ord a, Eq b)
          => Map a b  -- old
          -> Map a b  -- new
          -> Changes a b
compareTo old new = Changes added changed deleted
  where added = new \\ old
        changed = filterWithKey (\k v -> old ! k /= v) $ intersection new old
        deleted = keys $ old \\ new

prettyPrintChanges :: Changes SongName URL -> String
prettyPrintChanges Changes {..} = intercalate "\n" . catMaybes $ [newSongs, modifiedSongs, deletedSongs]
  where newSongs, modifiedSongs, deletedSongs :: Maybe String
        newSongs = template "new songs" (keys new)
        modifiedSongs = template "to be reinstalled" (keys modified)
        deletedSongs = template "to be deleted" deleted
        template _ [] = Nothing
        template prompt items = Just $ printf "%s: [%s]" prompt (intercalate ", " items)

songsIn :: MonadIO m => FilePath -> ReaderT FileInfo m [String]  -- basenames
songsIn dir = do ext <- asks audioFileExt
                 files <- liftIO $ listDirectory dir
                 let audios = filter (ext `isExtensionOf`) files
                 return $ map takeBaseName audios
