{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | High-level description of the Bristol Transport API
module BristolTransportApi
( baseUrl
, createApi
) where

import Data.Proxy
import Data.Text
import Servant.API (Header, (:>), (:<|>))
import Servant.Client (BaseUrl(..), Scheme( Https ), client)

import BristolTransportApi.ImportSources
import BristolTransportApi.TransitStops

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "bristol.api.urbanthings.io" 443 "/api/v2.0"

type StaticApis = "static" :>
  (    ImportSourcesApi
  :<|> TransitStopsApi
  )

type BristolTransportApi = Header "X-Api-Key" Text :>
  StaticApis

proxyApi :: Proxy BristolTransportApi
proxyApi = Proxy

createApi = client proxyApi
