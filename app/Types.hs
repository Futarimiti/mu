module Types where

import Data.Aeson
import Data.Data
import Data.Default
import Data.Functor.Barbie
import Data.Functor.Identity
import Data.List.NonEmpty
import GHC.Generics

data PlayOrder = Sequential | Shuffle
  deriving (Show, Eq, Generic, Data)

data SomeWithOptsOrAll opts a = Given opts (NonEmpty a) | All
  deriving (Show, Eq, Generic, Data)

type SomeOrAll = SomeWithOptsOrAll ()

type URL = String

data TrackInfo = TrackInfo
  { name :: String
  , url  :: URL
  } deriving (Show, Eq, Generic, Data)

instance FromJSON TrackInfo

data Playlist = Playlist
  { name   :: String
  , tracks :: [String]
  } deriving (Show, Eq, Generic, Data)

instance FromJSON Playlist

data ConfigOf f = Config
  { playlists :: f [Playlist]
  , library   :: f [TrackInfo]
  } deriving (Generic)

instance FunctorB ConfigOf
instance ApplicativeB ConfigOf
instance TraversableB ConfigOf
instance ConstraintsB ConfigOf
deriving instance AllBF Show f ConfigOf => Show (ConfigOf f)
deriving instance AllBF Eq f ConfigOf => Eq (ConfigOf f)
deriving instance (Typeable f, AllBF Data f ConfigOf) => Data (ConfigOf f)

-- | User config
type Config = ConfigOf Identity

instance Default Config where
  def = Config (Identity []) (Identity [])

-- | Raw input - could lack any fields
type RawConfig = ConfigOf Maybe

instance FromJSON RawConfig

data Command
  = PlaySome (SomeWithOptsOrAll PlayOrder String)
  | PlayList PlayOrder String
  | RemoveDownload (SomeOrAll String)
  | EnsureDownload (SomeOrAll String)
  deriving (Show, Eq, Generic, Data)

-- command-line options
data Options = Options
  { verbose  :: Bool
  , confFile :: Maybe FilePath
  , command  :: Command
  } deriving (Show, Eq, Generic, Data)
