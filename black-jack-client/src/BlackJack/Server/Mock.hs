{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module BlackJack.Server.Mock where

import BlackJack.Server (Host (..), InitResult (..), IsChain (..), Server (..))
import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON, decode)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Sum (..))
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import Network.HTTP.Simple (
  getResponseBody,
  getResponseStatusCode,
  httpJSON,
  httpLBS,
  parseRequest,
  setRequestBodyJSON,
 )
import Test.QuickCheck (Arbitrary (..), getPositive)

withMockServer :: String -> Host -> (Server MockChain IO -> IO ()) -> IO ()
withMockServer myId myHost k =
  newMockServer (pack myId) myHost >>= k

newMockServer :: Text -> Host -> IO (Server MockChain IO)
newMockServer myId myHost = do
  cnx <- connectToServer "localhost" 56789 myId myHost
  pure $
    Server
      { initHead = \peers -> do
          HeadId{headId} <- sendInit cnx peers
          pure $ checkInit cnx headId
      , commit = error "undefined"
      }

data MockServerError = MockServerError String
  deriving (Eq, Show)

instance Exception MockServerError

connectToServer :: String -> Int -> Text -> Host -> IO Host
connectToServer host port myId myHost = do
  request <- parseRequest $ "POST http://" <> host <> ":" <> show port <> "/connect/" <> unpack myId
  response <- httpLBS $ setRequestBodyJSON myHost request
  when (getResponseStatusCode response /= 200) $ throwIO $ MockServerError ("Failed to register with id " <> show myId)
  pure $ Host{host = pack host, port}

data HeadId = HeadId {headId :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

sendInit :: Host -> [Text] -> IO HeadId
sendInit Host{host, port} peers = do
  request <- parseRequest $ "POST http://" <> unpack host <> ":" <> show port <> "/init"
  response <- httpJSON $ setRequestBodyJSON peers request
  when (getResponseStatusCode response /= 200) $ throwIO $ MockServerError ("Failed to init head for peers " <> show peers)
  pure $ getResponseBody response

checkInit :: Host -> Text -> IO (InitResult MockChain)
checkInit Host{host, port} headId = do
  request <- parseRequest $ "GET http://" <> unpack host <> ":" <> show port <> "/init/" <> unpack headId
  response <- httpLBS request
  case getResponseStatusCode response of
    201 ->
      pure $
        InitDone headId $
          fromMaybe (error $ "failed to decode list of peers: " <> show (getResponseBody response)) $
            decode (getResponseBody response)
    202 -> pure InitPending
    _ ->
      pure $
        InitFailed $
          fromMaybe (error $ "failed to decode response: " <> show (getResponseBody response)) $
            decode (getResponseBody response)

data MockChain = MockChain

newtype MockCoin = MockCoin Integer
  deriving newtype (Eq, Show, ToJSON, FromJSON)
  deriving (Semigroup, Monoid) via Sum Integer

instance Arbitrary MockCoin where
  arbitrary = MockCoin . getPositive <$> arbitrary
  shrink (MockCoin c) = MockCoin <$> shrink c

newtype MockParty = Party Text
  deriving newtype (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

instance IsChain MockChain where
  type Party MockChain = MockParty
  type Coin MockChain = MockCoin

  partyId (Party s) = s

  coinValue (MockCoin c) = c
