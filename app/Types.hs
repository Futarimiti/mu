module Types where

import Data.Text (Text)

-- | Command and options/args
type Command = [String]
type SongName = String
type URL = FilePath
type OS = String
type ErrorMessage = Text

