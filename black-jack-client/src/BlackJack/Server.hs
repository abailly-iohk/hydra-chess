{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module BlackJack.Server where

import BlackJack.Game (BlackJack, Play)
import Control.Exception (Exception)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (arbitrary), vectorOf)

data Message = Ping
  deriving (Eq, Show)

data ServerException = ServerException {reason :: Text}
  deriving (Eq, Show)

instance Exception ServerException

class
  ( Show (Coin c)
  , Eq (Coin c)
  , Monoid (Coin c)
  , ToJSON (Coin c)
  , FromJSON (Coin c)
  , Show (Party c)
  , Eq (Party c)
  , ToJSON (Party c)
  , FromJSON (Party c)
  ) =>
  IsChain c
  where
  -- | Type of parties for the given chain.
  type Party c

  -- | Type of coins for the given chain.
  type Coin c

  -- | Retrieve a (unique for this chain) identifier for a given party.
  partyId :: Party c -> Text

  -- | Retrieve the amount in a coin.
  coinValue :: Coin c -> Integer

newtype HeadId = HeadId {headId :: Text}
  deriving newtype (Eq, Show, Ord, IsString, ToJSON, FromJSON)

instance Arbitrary HeadId where
  arbitrary = HeadId . decodeUtf8 . Hex.encode . BS.pack <$> vectorOf 16 arbitrary

-- | A handle to some underlying server for a single Head.
data Server c m = Server
  { -- | Initialises a head with given parties.
    -- Might throw an exception if something goes wrong before hitting the
    -- underlying chain.
    initHead :: [Text] -> m HeadId
  , -- | Commit some value to the given head.
    -- The server is responsible for finding a suitable `Coin` that will fit the
    -- amount funded.
    -- Might throw a `ServerException` if something goes wrong.
    commit :: Integer -> HeadId -> m ()
  , -- | Poll server for latest `FromChain` messages available.
    -- Takes the first event to retrieve and the maximum number of elements to send back.
    -- It will return 0 or more messages, depending on what's available, and the index
    -- of the last known message.
    poll :: Integer -> Integer -> m (Indexed c)
  }

data FromChain c
  = HeadCreated {headId :: HeadId, parties :: [Party c]}
  | FundCommitted {headId :: HeadId, party :: Party c, coin :: Coin c}
  | HeadOpened {headId :: HeadId, game :: BlackJack, plays :: [Play]}
  | GameChanged {headId :: HeadId, game :: BlackJack, plays :: [Play]}
  deriving (Generic)

deriving instance IsChain c => Show (FromChain c)
deriving instance IsChain c => Eq (FromChain c)
deriving instance IsChain c => ToJSON (FromChain c)
deriving instance IsChain c => FromJSON (FromChain c)

data Indexed c = Indexed {lastIndex :: Integer, events :: [FromChain c]}
  deriving (Generic)

deriving instance IsChain c => Show (Indexed c)
deriving instance IsChain c => Eq (Indexed c)
deriving instance IsChain c => ToJSON (Indexed c)
deriving instance IsChain c => FromJSON (Indexed c)

data Host = Host {host :: Text, port :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
