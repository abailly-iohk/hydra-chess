{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module HttpServer where

import BlackJack.Server (Host)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO)
import Control.Monad ((>=>))
import Data.Aeson (FromJSON, ToJSON, Value (Object), decode, eitherDecode, encode, toJSON)
import Data.Aeson.KeyMap (insert)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String (IsString (fromString))
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Time (getCurrentTime)
import qualified GHC.Clock
import GHC.Generics (Generic)
import Network.HTTP.Types (badRequest400, methodNotAllowed405, notFound404, ok200, statusCode)
import Network.Wai (Application, Middleware, ResponseReceived, pathInfo, requestBody, requestMethod, responseLBS, responseStatus, strictRequestBody)
import qualified Network.Wai.Handler.Warp as Warp

data HttpLog
  = HttpServerListening {host :: Text, port :: Int}
  | HttpRequest {path :: Text, method :: Text}
  | HttpResponse {status :: Int, time :: Double}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

httpServer host port =
  newTVarIO (Chain mempty) >>= Warp.runSettings settings . logRequest . app
 where
  logRequest :: Middleware
  logRequest app req send = do
    start <- GHC.Clock.getMonotonicTime
    doLog
      HttpRequest
        { path = "/" <> Text.intercalate "/" (pathInfo req)
        , method = decodeUtf8 (requestMethod req)
        }
    app req $ \res -> do
      result <- send res
      end <- GHC.Clock.getMonotonicTime
      let time = end - start / 1_000_000_000
      let status = statusCode $ responseStatus res
      doLog HttpResponse{status, time}
      pure result
   where
    method = decodeUtf8 (requestMethod req)
    path = pathInfo req

  settings =
    Warp.defaultSettings
      & Warp.setPort port
      & Warp.setHost (fromString host)
      & Warp.setServerName "mock-chain"
      & Warp.setBeforeMainLoop (doLog HttpServerListening{host = pack host, port})

data Chain = Chain
  { knownHosts :: Map Text Host
  }
  deriving stock (Eq, Show)

app :: TVar Chain -> Application
app state req send =
  route (requestMethod req) (pathInfo req)
 where
  route "POST" ["connect", hostId] = handleConnect state hostId
  route "POST" ["init"] = error "not implemented"
  route "GET" ["init", headId] = error "not implemented"
  route "GET" _ = send $ responseLBS notFound404 [] ""
  route method _ = send $ responseLBS methodNotAllowed405 [] ""

  handleConnect state hostId = do
    decoded <- eitherDecode <$> strictRequestBody req
    case decoded of
      Left err -> send $ responseLBS badRequest400 [] "Invalid host info body"
      Right hostInfo -> do
        atomically $ modifyTVar' state $ \chain@Chain{knownHosts} -> chain{knownHosts = Map.insert hostId hostInfo knownHosts}
        send $ responseLBS ok200 [] ""

doLog :: (ToJSON a) => a -> IO ()
doLog l = do
  ts <- getCurrentTime
  BS.putStr $ LBS.toStrict (logEntry ts <> "\n")
 where
  logEntry ts =
    let val = toJSON l
     in encode $ case val of
          Object obj -> Object $ insert "timestamp" (toJSON ts) obj
          v -> v
