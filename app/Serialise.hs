module Serialise ( deserialiseMap
                 , serialiseMap
                 ) where

import           Codec.Serialise (Serialise, readFileDeserialise,
                                  writeFileSerialise)
import           Data.Map        (Map)

deserialiseMap :: (Ord a, Serialise a, Serialise b) => FilePath -> IO (Map a b)
deserialiseMap = readFileDeserialise

serialiseMap :: (Ord a, Serialise a, Serialise b) => FilePath -> Map a b -> IO ()
serialiseMap = writeFileSerialise
