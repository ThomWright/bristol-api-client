{-# LANGUAGE DeriveGeneric #-}

module BristolTransportApi.ResponseBody
( ResponseBody
) where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics

data ResponseBody theRealData = ResponseBody
  { _success :: Maybe Bool
  , _localizedErrorMessage :: Maybe String
  , _data :: Maybe theRealData
  } deriving (Generic, Show)

instance ToJSON a => ToJSON (ResponseBody a) where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop 1 }

instance FromJSON a => FromJSON (ResponseBody a) where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 1 }
