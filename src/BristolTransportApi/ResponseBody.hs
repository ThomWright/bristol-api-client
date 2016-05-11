{-# LANGUAGE DeriveGeneric #-}

module BristolTransportApi.ResponseBody
( ResponseBody(..)
) where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics

-- | Response body shape
data ResponseBody theRealData = ResponseBody
  { _success :: Maybe Bool
    -- ^ Indicates either success or a 'soft failure' of the operation
  , _localizedErrorMessage :: Maybe String
    -- ^ An error message describing why a soft failure occurred
    --
    -- Where possible, this will be localized based upon the Accept-Language header passed to the API.
    -- If Success is TRUE then this field will be omitted from the response.
  , _data :: Maybe theRealData
    -- ^ Contains the main data of the API response. The Type of the data is endpoint-specific
  } deriving (Generic, Show)

instance ToJSON a => ToJSON (ResponseBody a) where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop 1 }

instance FromJSON a => FromJSON (ResponseBody a) where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 1 }
