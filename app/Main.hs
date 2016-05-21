
module Main where

import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as LazyByteStr
import Data.Text
import Data.Text.Encoding
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment

import Servant.API

import BristolTransportApi (baseUrl, createApi)

main :: IO ()
main = do
  apiKey <- lookupEnv "BRISTOL_API_KEY"
  case apiKey of
    Just key -> run (pack key)
    Nothing -> putStrLn "No environment variable for BRISTOL_API_KEY"

prettyJson :: ToJSON a => a -> String
prettyJson d = unpack $ decodeUtf8 $ LazyByteStr.toStrict (encodePretty d)

run :: Text -> IO ()
run apiKey = do
  let (importsources :<|> _) = createApi (Just apiKey)
  manager <- newManager tlsManagerSettings
  res <- runExceptT (importsources manager baseUrl)
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right body -> putStrLn $ prettyJson body
