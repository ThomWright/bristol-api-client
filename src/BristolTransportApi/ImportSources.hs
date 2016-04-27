{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module BristolTransportApi.ImportSources
( ImportSourcesApi
, ImportSourcesResponse
) where

import Data.Aeson
import Data.Aeson.Types
import Data.Text
import GHC.Generics
import Servant.API

import BristolTransportApi.ResponseBody

data ImportSource = ImportSource
  { name :: Maybe String
  , importSourceID :: Maybe String
  , attributionLabel :: Maybe String
  , attributionImageURL :: Maybe String
  , attributionNotes :: Maybe String
  , comments :: Maybe String
  , sourceInfoURL :: Maybe String
  , sourceDataURL :: Maybe String
  } deriving (Generic, Show)

instance ToJSON ImportSource
instance FromJSON ImportSource

type ImportSourcesResponse = ResponseBody [ImportSource]

type ImportSourcesApi =  "importsources" :> Header "X-Api-Key" Text :> Get '[JSON] ImportSourcesResponse
