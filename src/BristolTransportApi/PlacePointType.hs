{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module BristolTransportApi.PlacePointType
( PlacePointType(..)
) where

import Data.Aeson
import Data.Data
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import GHC.Generics
import BristolTransportApi.Internal.EnumTable (createTable)

-- | Used as an indicator of what a PlacePoint represents.
--
-- In cases where the more detailed derived class of TransitStop is returned, this should be ignored and the StopMode field should be inspected instead.
data PlacePointType =
  Place
  | Road
  | TransitStop
  | Postcode
  | LatLng
  | Locality
  | POI
  | TransitStopTram
  | TransitStopSubway
  | TransitStopRail
  | TransitStopBus
  | TransitStopFerry
  | TransitStopCableCar
  | TransitStopGondola
  | TransitStopFunicular
  | TransitStopAir
  | CycleHireDock
  | CarParkingSpace
  deriving (Eq, Show, Data, Typeable, Generic)

instance ToJSON PlacePointType where
  toJSON = toJSON . fromEnum
instance FromJSON PlacePointType where
  parseJSON = withScientific "Integral" (return . toEnum . truncate)

instance Enum PlacePointType where
  fromEnum = fromJust . flip lookup table
  toEnum = fromJust . flip lookup (map swap table)

toNumber :: PlacePointType -> Int
toNumber enum = case enum of
  Place -> 0
  Road -> 1
  TransitStop -> 2
  Postcode -> 3
  LatLng -> 4
  Locality -> 5
  POI -> 6
  TransitStopTram -> 100
  TransitStopSubway -> 101
  TransitStopRail -> 102
  TransitStopBus -> 103
  TransitStopFerry -> 104
  TransitStopCableCar -> 105
  TransitStopGondola -> 106
  TransitStopFunicular -> 107
  TransitStopAir -> 108
  CycleHireDock -> 111
  CarParkingSpace -> 112

table :: [(PlacePointType, Int)]
table = createTable toNumber Place
