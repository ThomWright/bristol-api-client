{-# LANGUAGE DeriveGeneric #-}

module ImportSources where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics

import ApiResponse

data ImportSource = ImportSource
  { name :: Maybe String
  , importSourceID :: Maybe String
  , attributionLabel :: Maybe String
  , attributionImageURL :: Maybe String
  , attributionNotes :: Maybe String
  , comments :: Maybe String
  , sourceInfoURL :: Maybe String
  , sourceDataURL :: Maybe String
  } deriving (Generic)

instance ToJSON ImportSource
instance FromJSON ImportSource

type ImportSourceResponse = ApiResponse [ImportSource]
