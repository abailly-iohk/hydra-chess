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

import Control.Exception (Exception)
import Data.Aeson (FromJSON, ToJSON)
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)

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
    -- It will return 0 or more messages, depending on what's available.
    poll :: m [FromChain c]
  }

data FromChain c
  = HeadCreated {headId :: Text, parties :: [Party c]}
  | FundCommitted {headId :: Text, party :: Party c, coin :: Coin c}
  deriving (Generic)

deriving instance IsChain c => Show (FromChain c)
deriving instance IsChain c => Eq (FromChain c)
deriving instance IsChain c => ToJSON (FromChain c)
deriving instance IsChain c => FromJSON (FromChain c)

data Host = Host {host :: Text, port :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
