{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module BlackJack.Server where

import Control.Exception (Exception)
import Data.Text (Text)

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
  { -- | Connects to given party.
    -- Might throw a `ServerException`.
    connect :: Text -> m (Party c)
  , -- | Initialises a head with given parties.
    -- Those parties must have been connected to first.
    -- Returns an action that can be used to check whether or not the initialisation is
    -- done.
    -- Might throw a `ServerException`.
    initHead :: [Party c] -> m (m InitResult)
  , -- | Commit some value to the given head.
    -- The server is responsible for finding a suitable `Coin` that will fit the
    -- amount funded.
    commit :: Integer -> m (m (CommitResult c))
  }

data InitResult
  = InitDone {headId :: Text}
  | InitPending
  | InitFailed {reason :: Text}
  deriving stock (Eq, Show)

data CommitResult c = CommitDone {coin :: Coin c}

deriving instance IsChain c => Show (CommitResult c)
deriving instance IsChain c => Eq (CommitResult c)
