{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | High-level description of the Bristol Transport API
module BristolTransportApi
( baseUrl
, importsources
, transitStops
) where

import Data.Proxy
import Servant.API
import Servant.Client

import BristolTransportApi.ImportSources
import BristolTransportApi.TransitStops

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "bristol.api.urbanthings.io" 443 "/api/v2.0"

type StaticApis = "static" :>
  ImportSourcesApi
  :<|> TransitStopsApi

type BristolTransportApi =
  StaticApis
  -- :<|> RealTimeApis

proxyApi :: Proxy BristolTransportApi
proxyApi = Proxy

{-otherTopLevelPath :<|> -}
importsources :<|> transitStops = client proxyApi
