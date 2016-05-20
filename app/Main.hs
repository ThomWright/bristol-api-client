
module Main where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BSL
import Data.Text
import Data.Text.Encoding
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment

import BristolTransportApi (baseUrl, importsources, transitStops)

main :: IO ()
main = do
  apiKey <- lookupEnv "BRISTOL_API_KEY"
  case apiKey of
    Just key -> run (pack key)
    Nothing -> putStrLn "No environment variable for BRISTOL_API_KEY"

prettyJson :: ToJSON a => a -> String
prettyJson d = unpack $ decodeUtf8 $ BSL.toStrict (encodePretty d)

run :: Text -> IO ()
run apiKey = do
  manager <- newManager tlsManagerSettings
  res <- runExceptT (importsources (Just apiKey) manager baseUrl)
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right body -> putStrLn $ prettyJson body
