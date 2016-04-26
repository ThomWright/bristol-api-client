{-# LANGUAGE DeriveGeneric #-}

module ApiResponse where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics

data ApiResponse theRealData = ApiResponse
  { _success :: Maybe Bool
  , _localizedErrorMessage :: Maybe String
  , _data :: Maybe theRealData
  } deriving (Generic)

instance ToJSON a => ToJSON (ApiResponse a) where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop 1 }

instance FromJSON a => FromJSON (ApiResponse a) where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 1 }
