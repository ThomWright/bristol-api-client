{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module BristolApi where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson
import Data.Aeson.Types
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Servant.API
import Servant.Client

import ImportSources
