{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module BlackJack.Server where

import Control.Exception (Exception)
import Data.Aeson (FromJSON, ToJSON)
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
  , Show (Party c)
  , Eq (Party c)
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

-- | A handle to some underlying server for a single Head.
data Server c m = Server
  { -- | Initialises a head with given parties.
    -- Returns an action that can be used to check whether or not the initialisation is
    -- done.
    initHead :: [Text] -> m (m (InitResult c))
  , -- | Commit some value to the given head.
    -- The server is responsible for finding a suitable `Coin` that will fit the
    -- amount funded.
    -- Returns an action that can be used to check whether or not the commit is done, or
    -- failed.
    commit :: Integer -> Text -> m (m (CommitResult c))
  , poll :: m (Maybe (FromChain c))
  }

data FromChain c = HeadCreated {headId :: Text, parties :: [Party c]}

deriving instance IsChain c => Show (FromChain c)
deriving instance IsChain c => Eq (FromChain c)

data InitResult c
  = InitDone {headId :: Text, parties :: [Party c]}
  | InitPending
  | InitFailed {reason :: Text}

deriving instance IsChain c => Show (InitResult c)
deriving instance IsChain c => Eq (InitResult c)

data CommitResult c
  = CommitDone {coin :: Coin c}
  | NoMatchingCoin {value :: Integer, coins :: [Coin c]}

deriving instance IsChain c => Show (CommitResult c)
deriving instance IsChain c => Eq (CommitResult c)

data Host = Host {host :: Text, port :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
