module Types where

import Data.Data
import Data.List.NonEmpty
import GHC.Generics

data PlayOrder = Sequential | Shuffle
  deriving (Show, Eq, Generic, Data)

data SomeWithOptsOrAll opts a = Given opts (NonEmpty a) | All
  deriving (Show, Eq, Generic, Data)

type SomeOrAll = SomeWithOptsOrAll ()

data Command
  = PlaySome (SomeWithOptsOrAll PlayOrder String)
  | PlayList PlayOrder String
  | RemoveDownload (SomeOrAll String)
  | EnsureDownload (SomeOrAll String)
  deriving (Show, Eq, Generic, Data)

-- command-line options
data Options = Options
  { verbose :: Bool
  , command :: Command
  } deriving (Show, Eq, Generic, Data)
