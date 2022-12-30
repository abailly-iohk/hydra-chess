{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module BlackJack.Server.Mock where

import BlackJack.Server (HeadId (HeadId), Host (..), Indexed, IsChain (..), Server (..))
import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON)
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

withMockServer :: MockParty -> (Server MockChain IO -> IO ()) -> IO ()
withMockServer myParty k =
  newMockServer myParty >>= k

newMockServer :: MockParty -> IO (Server MockChain IO)
newMockServer myParty = do
  cnx <- connectToServer "localhost" 56789 myParty
  pure $
    Server
      { initHead = sendInit cnx
      , commit = sendCommit cnx (pid myParty)
      , poll = pollEvents cnx
      }

data MockServerError = MockServerError String
  deriving (Eq, Show)

instance Exception MockServerError

connectToServer :: String -> Int -> MockParty -> IO Host
connectToServer host port myParty = do
  request <- parseRequest $ "POST http://" <> host <> ":" <> show port <> "/connect/" <> unpack (pid myParty)
  response <- httpLBS $ setRequestBodyJSON myParty request
  when (getResponseStatusCode response /= 200) $ throwIO $ MockServerError ("Failed to register with id " <> show myParty)
  pure $ Host{host = pack host, port}

sendInit :: Host -> [Text] -> IO HeadId
sendInit Host{host, port} peers = do
  request <- parseRequest $ "POST http://" <> unpack host <> ":" <> show port <> "/init"
  response <- httpJSON $ setRequestBodyJSON peers request
  when (getResponseStatusCode response /= 200) $ throwIO $ MockServerError ("Failed to init head for peers " <> show peers)
  pure $ getResponseBody response

sendCommit :: Host -> Text -> Integer -> HeadId -> IO ()
sendCommit Host{host, port} myId amount (HeadId headId) = do
  request <-
    parseRequest $
      "POST http://"
        <> unpack host
        <> ":"
        <> show port
        <> "/commit/"
        <> unpack headId
        <> "/"
        <> unpack myId
        <> "/"
        <> show amount
  response <- httpLBS request
  when (getResponseStatusCode response /= 200) $ throwIO $ MockServerError ("Failed to commit for peers " <> show myId)

pollEvents :: Host -> Integer -> Integer -> IO (Indexed MockChain)
pollEvents Host{host, port} index num = do
  request <- parseRequest $ "GET http://" <> unpack host <> ":" <> show port <> "/events/" <> show index <> "/" <> show num
  getResponseBody <$> httpJSON request

data MockChain = MockChain

newtype MockCoin = MockCoin Integer
  deriving newtype (Eq, Show, ToJSON, FromJSON)
  deriving (Semigroup, Monoid) via Sum Integer

instance Arbitrary MockCoin where
  arbitrary = MockCoin . getPositive <$> arbitrary
  shrink (MockCoin c) = MockCoin <$> shrink c

data MockParty = Party {host :: Host, pid :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance IsChain MockChain where
  type Party MockChain = MockParty
  type Coin MockChain = MockCoin

  partyId Party{pid} = pid

  coinValue (MockCoin c) = c
