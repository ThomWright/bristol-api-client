{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module BristolTransportApi
( baseUrl
, importsources
) where

import Data.Proxy
import Servant.API
import Servant.Client

import BristolTransportApi.ImportSources

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "bristol.api.urbanthings.io" 443 "/api/v2.0"

type StaticApis = "static" :>
  ImportSourcesApi

type BristolTransportApi =
  StaticApis
  -- :<|> RealTimeApis

proxyApi :: Proxy BristolTransportApi
proxyApi = Proxy

{-otherTopLevelPath :<|> -}
importsources = client proxyApi
