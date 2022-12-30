{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HttpServer where

import BlackJack.Server (FromChain (HeadCreated), HeadId (HeadId), Host, Indexed (..))
import BlackJack.Server.Mock (MockChain, MockParty)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Monad (replicateM, (>=>))
import Data.Aeson (FromJSON, ToJSON, Value (Object), decode, eitherDecode, encode, toJSON)
import Data.Aeson.KeyMap (insert)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.String (IsString (fromString))
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Time (getCurrentTime)
import qualified GHC.Clock
import GHC.Generics (Generic)
import Network.HTTP.Types (badRequest400, methodNotAllowed405, notFound404, ok200, statusCode)
import Network.Wai (Application, Middleware, ResponseReceived, pathInfo, requestBody, requestMethod, responseLBS, responseStatus, strictRequestBody)
import qualified Network.Wai.Handler.Warp as Warp
import System.Random.Stateful (applyAtomicGen, globalStdGen, uniformR)

data HttpLog
  = HttpServerListening {host :: Text, port :: Int}
  | HttpRequest {path :: Text, method :: Text}
  | HttpResponse {status :: Int, time :: Double}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

httpServer host port =
  newTVarIO (Chain mempty mempty mempty) >>= Warp.runSettings settings . logRequest . app
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
      let time = (end - start) * 1_000
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

data HeadState = Initialising [MockParty]
  deriving (Eq, Show)

data Chain = Chain
  { knownHosts :: Map Text MockParty
  , heads :: Map HeadId HeadState
  , events :: Seq (FromChain MockChain)
  }
  deriving stock (Eq, Show)

app :: TVar Chain -> Application
app state req send =
  route (requestMethod req) (pathInfo req)
 where
  route "POST" ["connect", hostId] = handleConnect hostId
  route "POST" ["init"] = handleInit
  route "GET" ["events", idx, num] = handleEvents (read $ unpack idx) (read $ unpack num)
  route "GET" _ = send $ responseLBS notFound404 [] ""
  route method _ = send $ responseLBS methodNotAllowed405 [] ""

  handleConnect hostId = do
    decoded <- eitherDecode <$> strictRequestBody req
    case decoded of
      Left err -> send $ responseLBS badRequest400 [] "Invalid host info body"
      Right hostInfo -> do
        atomically $ modifyTVar' state $ \chain@Chain{knownHosts} -> chain{knownHosts = Map.insert hostId hostInfo knownHosts}
        send $ responseLBS ok200 [] ""

  handleInit = do
    decoded <- eitherDecode <$> strictRequestBody req
    case decoded of
      Left err -> send $ responseLBS badRequest400 [] "Invalid peers body"
      Right (peers :: [Text]) -> do
        newHeadId <- mkNewHeadId
        atomically $ do
          chain@Chain{heads, knownHosts, events} <- readTVar state
          let headPeers = mapMaybe (`Map.lookup` knownHosts) peers
              chain' =
                chain
                  { heads = Map.insert newHeadId (Initialising headPeers) heads
                  , events = events |> HeadCreated newHeadId headPeers
                  }
          writeTVar state chain'
        send $ responseLBS ok200 [] $ encode newHeadId

  handleEvents idx num = do
    Chain{events} <- readTVarIO state
    let indexed =
          Indexed
            { lastIndex = fromIntegral $ length events
            , events = toList $ Seq.take num $ Seq.drop idx events
            }
    send $ responseLBS ok200 [] $ encode indexed

mkNewHeadId :: IO HeadId
mkNewHeadId = HeadId . pack . fmap toChar <$> replicateM 32 randomNibble
 where
  randomNibble = applyAtomicGen (uniformR (0x0, 0xF)) globalStdGen
  toChar :: Int -> Char
  toChar 0 = '0'
  toChar 1 = '1'
  toChar 2 = '2'
  toChar 3 = '3'
  toChar 4 = '4'
  toChar 5 = '5'
  toChar 6 = '6'
  toChar 7 = '7'
  toChar 8 = '8'
  toChar 9 = '9'
  toChar 10 = 'a'
  toChar 11 = 'b'
  toChar 12 = 'c'
  toChar 13 = 'd'
  toChar 14 = 'e'
  toChar 15 = 'f'
  toChar _ = error "unexpected value"

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
