{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module BristolTransportApi.TransitStops
( TransitStopsApi
, TransitStopsResponse
, TransitStop(..)
) where

import Data.Aeson
import GHC.Generics
import Servant.API

import BristolTransportApi.ResponseBody
import BristolTransportApi.VehicleType
import BristolTransportApi.PlacePointType

-- | Retrieve a list of one or more 'TransitStop' objects
--
-- This method is used to retrieve matching TransitStop objects - bus stops, train stations, car parks, etc.
-- These can be retrieved based upon location by supplying a lat/lng bounding box.
-- Alternatively (or in parallel) the list can be filtered by matching against the name of the transit stop or the dataset to which it belongs.
--
-- Max size of the bounding box is 20km x 20km
--
-- Query Parameters:
--
-- [@minLat@] The minimum lat of the bounding box.
-- [@maxLat@] The maximum latitude of the bounding box.
-- [@minLng@] The minimum longitude of the bounding box.
-- [@maxLng@] The maximum longitude of the bounding box.
-- [@centerLat@] The latitude of a center point to search for stops nearby.
-- [@centerLng@] The longitude of a center point to search for stops nearby.
-- [@radius@] The radius around the center point to search for stops nearby. Specified in metres, clamped to a maximum value of 10km.
-- [@maxResults@] The maximum number of results to return. Defaults to 1000.
-- [@stopModes@] A comma-delimited list of stop modes used to filter the list of TransitStops returned.
-- [@stopName@] The name of particular transit stop(s) to search for. Specify six or more characters. Searches are case insensitive.
-- [@stopIDs@] A comma-delimited list of primary codes of transit stops to search for.
-- [@importSource@] Optionally filter the returned list to include only TransitStops from the Dataset with this ImportSource ID (see /datasets endpoint).
type TransitStopsApi =  (  "transitstops"
                        :> QueryParam "minLat" Double
                        :> QueryParam "maxLat" Double
                        :> QueryParam "minLng" Double
                        :> QueryParam "maxLng" Double
                        :> QueryParam "centerLat" Double
                        :> QueryParam "centerLng" Double
                        :> QueryParam "radius" Double
                        :> QueryParam "maxResults" Integer
                        :> QueryParam "stopModes" String
                        :> QueryParam "stopName" String
                        :> QueryParam "stopIDs" String
                        :> QueryParam "importSource" String
                        :> Get '[JSON] TransitStopsResponse
                        )

type TransitStopsResponse = ResponseBody [TransitStop]

data TransitStop = TransitStop
  { additionalCode :: Maybe String
    -- ^ A secondary code used to identify this TransitStop
    --
    -- Whereas the 'primaryCode' is often unique to this data ecosystem, a SecondaryCode a likely to be unique only within the agency or data system that supplied it.
    -- Not guaranteed to be globally unique and not implemented in all data sets.
  , smsCode :: Maybe String
    -- ^ A code used for the retrieval of real time data via SMS. Not implemented in all data sets
  , bearing :: Maybe Integer
    -- ^ The compass bearing of vehicles leaving this stop. Not implemented in all data sets
  , directionName :: Maybe String
    -- ^ A generic direction name for vehicles leaving this stop, e.g. [towards] 'Marble Arch'
  , stopIndicator :: Maybe String
    -- ^ A short textual series of numbers of letters to identify this stop in the physical world, e.g. 'A1'
    --
    -- In general these letters will be publicly displayed on or near the stop.
  , isClosed :: Maybe Bool
    -- ^ Indicates permanent or temporary closure of the stop
  , stopMode :: Maybe VehicleType
    -- ^The GTFS mode of the routes that usually serve this stop
  , importSource :: Maybe String
    -- ^ The import source ('BristolTransportApi.ImportSources.ImportSource') that this object was originally sourced from
  , primaryCode :: Maybe String
    -- ^ A globally unique identifier representing this object
  , placePointType :: Maybe PlacePointType
    -- ^ The type of 'PlacePoint' - e.g. a transit stop, road, POI, Lat/Lng coordinate, etc.
    --
    -- In some cases, this can be useful to determine the type of a Transit Stop when the fuller derived class (TransitStop) is not returned from the server.
  , localityName :: Maybe String
    -- ^ The locality where this item is located. Can be blank
  , country :: Maybe String
    -- ^ The country where this item is located, in ISO 3166-1 alpha-2 format
  , hasResourceStatus :: Maybe Bool
    -- ^ If this is set to TRUE, the 'PlacePoint' represents a resource such as a car park or bicycle dock.
    --
    -- Further information as to the current status of the resource can be obtained by making an additional call to the API to obtain the current ResourceStatus for this 'PlacePointType'.
  , subClassType :: Maybe String
    -- ^ This field is used to aid client-side deserialisation with relation to inheritance
    --
    -- For example, if classType is 'TransitStop' then the 'PlacePoint' can be deserialized into its more complex subclass of 'TransitStop'.
    -- However, if it is NULL (or "PlacePoint") then the object is of type 'PlacePoint'.
  , name :: Maybe String
    -- ^ The item's name, in its native Locale
  , lat :: Maybe Double
    -- ^ The latitudinal component of the location of this item
  , lng :: Maybe Double
    -- ^ The longitudinal component of the location of this item
  } deriving (Generic, Show)

instance ToJSON TransitStop
instance FromJSON TransitStop
