module Types where

import Control.Monad.Catch
import Data.Aeson
import Data.Data
import Data.Default
import Data.Functor.Barbie
import Data.Functor.Identity
import Data.List.NonEmpty
import Data.Map              (Map)
import GHC.Generics

data PlayOrder = Sequential | Shuffle
  deriving (Show, Eq, Generic, Data)

data SomeWithOptsOrAll opts a = Given opts (NonEmpty a) | All
  deriving (Show, Eq, Generic, Data)

type SomeOrAll = SomeWithOptsOrAll ()

type URL = String

type Playlists = Map PlaylistRef PlaylistInfo

type Library = Map TrackRef TrackInfo

data TrackInfo = TrackInfo
  { name :: Maybe String
  , url  :: URL
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TrackInfo

data PlaylistInfo = PlaylistInfo
  { name   :: String
  , tracks :: [String]
  } deriving (Show, Eq, Generic, Data)

instance FromJSON PlaylistInfo

data ConfigF f = Config
  { playlists :: f Playlists
  , library   :: f Library
  } deriving (Generic)

instance FunctorB ConfigF
instance ApplicativeB ConfigF
instance TraversableB ConfigF
instance ConstraintsB ConfigF
deriving instance AllBF Show f ConfigF => Show (ConfigF f)
deriving instance AllBF Eq f ConfigF => Eq (ConfigF f)
deriving instance (Typeable f, AllBF Data f ConfigF) => Data (ConfigF f)

-- | Validated config
type Config = ConfigF Identity

instance Default Config

-- | Raw input - could lack any fields
type RawConfig = ConfigF Maybe

instance FromJSON RawConfig

data Command
  = PlaySome (SomeWithOptsOrAll PlayOrder String)
  | PlayList PlayOrder String
  | RemoveDownload (SomeOrAll String)
  | EnsureDownload (SomeOrAll String)
  | ListSome (SomeOrAll String)
  deriving (Show, Eq, Generic, Data)

-- command-line options
data Options = Options
  { verbose  :: Bool
  , confFile :: Maybe FilePath
  , command  :: Command
  } deriving (Show, Eq, Generic, Data)

-- for reader pattern
data AppEnv = AppEnv
  { verbose :: Bool
  , conf    :: Config
  , command :: Command
  } deriving (Show, Eq, Generic, Data)

type TrackRef = String

type PlaylistRef = String

newtype TrackNotFoundException
  = TrackNotFoundException { ref :: TrackRef }
  deriving (Show, Eq, Generic, Data)

instance Exception TrackNotFoundException

newtype PlaylistNotFoundException
  = PlaylistNotFoundException { ref :: PlaylistRef }
  deriving (Show, Eq, Generic, Data)

instance Exception PlaylistNotFoundException
