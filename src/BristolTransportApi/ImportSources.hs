{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module BristolTransportApi.ImportSources
( ImportSourcesApi
, ImportSourcesResponse
, ImportSource(..)
) where

import Data.Aeson
import Data.Aeson.Types
import Data.Text
import GHC.Generics
import Servant.API

import BristolTransportApi.ResponseBody

-- | Retrieve a list of all import sources (i.e. datasets) in the system
--
-- This method is used to discover the externally imported data sets that make up the data within the system.
-- Each data set is identified by an 'ImportSourceID', which can be used to retrieve lists of Transit Agencies, TransitStops and more.
type ImportSourcesApi =  "importsources" :> Header "X-Api-Key" Text :> Get '[JSON] ImportSourcesResponse

type ImportSourcesResponse = ResponseBody [ImportSource]

-- | Import source data
data ImportSource = ImportSource
  { name :: Maybe String
    -- ^ The name of the dataset (where available)
  , importSourceID :: Maybe String
    -- ^ The unique identifier representing the source of the data. Used as a foreign key in some objects, e.g. TransitAgency
  , attributionLabel :: Maybe String
  , attributionImageURL :: Maybe String
    -- ^ An attribution graphic for the data
    --
    -- Clients MUST display either this label or the string contained at 'attributionLabel' to conform with the Terms and Conditions of using the API.
  , attributionNotes :: Maybe String
    -- ^ Optional guidance explaining how the AttributionText and AttributionImageURL should be displayed
  , comments :: Maybe String
    -- ^ Optional comments to clarify the source of the data or explain its origin
  , sourceInfoURL :: Maybe String
    -- ^ Optional URL to a web page describing the source of the data
  , sourceDataURL :: Maybe String
    -- ^ Optional URL linking to the external data itself
  } deriving (Generic, Show)

instance ToJSON ImportSource
instance FromJSON ImportSource
