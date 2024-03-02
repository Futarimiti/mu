{-# LANGUAGE RecordWildCards #-}

module Update.Changes where

import           Data.List   (intercalate)
import           Data.Map    (Map, filterWithKey, intersection, keys, (!), (\\))
import           Data.Maybe  (catMaybes)
import           Data.Text   (Text)
import qualified Data.Text   as T
import           Lib
import           Text.Printf (printf)

data Changes a b = Changes { new      :: Map a b
                           , modified :: Map a b
                           , deleted  :: [a]
                           } deriving (Show, Eq)

noChange :: Changes a b -> Bool
noChange Changes {..} = null new && null modified && null deleted

compareTo :: (Ord a, Eq b)
          => Map a b  -- old
          -> Map a b  -- new
          -> Changes a b
compareTo old new = Changes added changed deleted
  where added = new \\ old
        changed = filterWithKey (\k v -> old ! k /= v) $ intersection new old
        deleted = keys $ old \\ new

prettyPrintChanges :: Changes SongName URL -> Text
prettyPrintChanges Changes {..} = T.unlines . catMaybes $ [newSongs, modifiedSongs, deletedSongs]
  where newSongs, modifiedSongs, deletedSongs :: Maybe Text
        newSongs = template "new songs" (keys new)
        modifiedSongs = template "to be reinstalled" (keys modified)
        deletedSongs = template "to be deleted" deleted
        template :: Text -> [SongName] -> Maybe Text
        template _ [] = Nothing
        template prompt items = Just $ T.pack $ printf "%s: [%s]" prompt (intercalate ", " items)

