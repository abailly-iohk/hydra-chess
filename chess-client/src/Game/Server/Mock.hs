{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Game.Server.Mock where

import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON (toJSON))
import Data.Semigroup (Sum (..))
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import Game.Server (
  Game (GamePlay),
  HeadId (HeadId),
  Host (..),
  Indexed,
  IsChain (..),
  Server (..),
 )
import Network.HTTP.Simple (
  getResponseBody,
  getResponseStatusCode,
  httpJSON,
  httpLBS,
  parseRequest,
  setRequestBodyJSON,
 )
import Test.QuickCheck (Arbitrary (..), arbitraryPrintableChar, getPositive, listOf)
import Test.QuickCheck.Gen (Gen)

withMockServer :: Game g => MockParty -> (Server g MockChain IO -> IO ()) -> IO ()
withMockServer myParty k =
  newMockServer myParty >>= k

newMockServer :: forall g . (Game g, ToJSON (GamePlay g)) => MockParty -> IO (Server g MockChain IO)
newMockServer myParty = do
  cnx <- connectToServer "localhost" 56789 myParty
  pure $
    Server
      { initHead = sendInit cnx
      , commit = sendCommit cnx (pid myParty)
      , poll = pollEvents cnx
      , play = playGame cnx (pid myParty)
      , newGame = restartGame cnx
      , closeHead = sendClose cnx
      }
 where
  playGame :: Host -> Text -> HeadId -> GamePlay g -> IO ()
  playGame Host{host, port} myId (HeadId hid) play = do
  request <- parseRequest $ "POST http://" <> unpack host <> ":" <> show port <> "/play/" <> unpack hid <> "/" <> unpack myId
  response <- httpLBS $ setRequestBodyJSON (toJSON play) request
  when (getResponseStatusCode response /= 200) $ throwIO $ MockServerError ("Failed to play " <> show play <> " for player " <> show myId)


newtype MockServerError = MockServerError String
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

restartGame :: Host -> HeadId -> IO ()
restartGame Host{host, port} (HeadId hid) = do
  request <- parseRequest $ "POST http://" <> unpack host <> ":" <> show port <> "/start/" <> unpack hid
  response <- httpLBS request
  when (getResponseStatusCode response /= 200) $ throwIO $ MockServerError ("Failed to restart game " <> unpack hid)

sendClose :: Host -> HeadId -> IO ()
sendClose Host{host, port} (HeadId hid) = do
  request <- parseRequest $ "POST http://" <> unpack host <> ":" <> show port <> "/close/" <> unpack hid
  response <- httpLBS request
  when (getResponseStatusCode response /= 200) $ throwIO $ MockServerError ("Failed to close head " <> unpack hid)

pollEvents :: Game g => Host -> Integer -> Integer -> IO (Indexed g MockChain)
pollEvents Host{host, port} index num = do
  request <- parseRequest $ "GET http://" <> unpack host <> ":" <> show port <> "/events/" <> show index <> "/" <> show num
  getResponseBody <$> httpJSON request

data MockChain = MockChain

newtype MockCoin = MockCoin Integer
  deriving newtype (Eq, Show, Num, ToJSON, FromJSON)
  deriving (Semigroup, Monoid) via Sum Integer

instance Arbitrary MockCoin where
  arbitrary = MockCoin . getPositive <$> arbitrary
  shrink (MockCoin c) = MockCoin <$> shrink c

data MockParty = Party {host :: Host, pid :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary MockParty where
  arbitrary = Party <$> arbitrary <*> somePid
   where
    somePid :: Gen Text
    somePid = pack <$> listOf arbitraryPrintableChar

instance IsChain MockChain where
  type Party MockChain = MockParty
  type Coin MockChain = MockCoin

  partyId Party{pid} = pid

  coinValue (MockCoin c) = c
