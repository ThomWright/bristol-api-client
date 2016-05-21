{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | High-level description of the Bristol Transport API
module BristolTransportApi
( ClientFactory
, baseUrl
, createApi
) where

import Data.Proxy
import Data.Text
import Network.HTTP.Client (Manager)
import Servant.API
import Servant.Common.Req (ClientM)
import Servant.Client

import BristolTransportApi.ImportSources
import BristolTransportApi.TransitStops
import BristolTransportApi.ResponseBody

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

type ClientFactory a = Manager -> BaseUrl -> ClientM (ResponseBody a)

createApi :: Maybe Text -> (    ClientFactory [ImportSource]
                           :<|> ClientFactory [TransitStop]
                           )
createApi = client proxyApi
