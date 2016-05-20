{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module BristolTransportApi.VehicleType
( VehicleType(..)
) where

import Data.Aeson
import Data.Data
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import GHC.Generics
import BristolTransportApi.Internal.EnumTable (createTable)

-- | Derived from GTFS, with non-included modes added. Also used to represent StopMode and RouteMode.
--
-- The VehicleType 'CableCar' has a special meaning with roots in San Francisco.
-- This represents a type of ground transportation similar to the European concept of a tram.
-- To refer to the airbourne form of transportation that conveys passengers upwards, suspended from a high cable, the VehicleType 'Gondola' should be used.
data VehicleType =
  Tram
  | Subway
  | Rail
  | Bus
  | Ferry
  | CableCar
  | Gondola
  | Funicular
  | Air
  | Walking
  | CycleOwned
  | CycleHired
  | Car
  | Coach
  | Multiple
  | Unset
  deriving (Eq, Show, Data, Typeable, Generic)

instance ToJSON VehicleType where
  toJSON = toJSON . fromEnum
instance FromJSON VehicleType where
  parseJSON = withScientific "Integral" (return . toEnum . truncate)

instance Enum VehicleType where
  fromEnum = fromJust . flip lookup table
  toEnum = fromJust . flip lookup (map swap table)

toNumber :: VehicleType -> Int
toNumber enum = case enum of
   Tram -> 0
   Subway -> 1
   Rail -> 2
   Bus -> 3
   Ferry -> 4
   CableCar -> 5
   Gondola -> 6
   Funicular -> 7
   Air -> 8
   Walking -> 9
   CycleOwned -> 10
   CycleHired -> 11
   Car -> 12
   Coach -> 13
   Multiple -> 9000
   Unset -> 9999

table :: [(VehicleType, Int)]
table = createTable toNumber Tram
