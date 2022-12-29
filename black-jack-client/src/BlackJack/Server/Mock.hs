{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module BlackJack.Server.Mock where

import BlackJack.Server (InitResult (..), IsChain (..), Server (..))
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

withMockServer :: String -> (Server MockChain IO -> IO ()) -> IO ()
withMockServer myId k =
  newMockServer (pack myId) >>= k

newMockServer :: Text -> IO (Server MockChain IO)
newMockServer myId = do
  cnx <- connectToServer "localhost" 56789 myId
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

type Host = (String, Int)

connectToServer :: String -> Int -> Text -> IO Host
connectToServer host port myId = do
  request <- parseRequest $ "http://" <> host <> ": " <> show port <> "/connect/" <> unpack myId
  response <- httpLBS request
  when (getResponseStatusCode response /= 200) $ throwIO $ MockServerError ("Failed to register with id " <> show myId)
  pure (host, port)

data HeadId = HeadId {headId :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

sendInit :: Host -> [Text] -> IO HeadId
sendInit (host, port) peers = do
  request <- parseRequest $ "POST http://" <> host <> ": " <> show port <> "/init"
  response <- httpJSON $ setRequestBodyJSON peers request
  when (getResponseStatusCode response /= 200) $ throwIO $ MockServerError ("Failed to init head for peers " <> show peers)
  pure $ getResponseBody response

checkInit :: Host -> Text -> IO (InitResult MockChain)
checkInit (host, port) headId = do
  request <- parseRequest $ "GET http://" <> host <> ": " <> show port <> "/init/" <> unpack headId
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
