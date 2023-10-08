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

module Game.Server where

import Control.Exception (Exception)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import Data.Kind (Type)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (arbitrary), vectorOf)

data Message = Ping
  deriving (Eq, Show)

newtype ServerException = ServerException {reason :: Text}
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

class
  ( Eq (GamePlay g)
  , Show (GamePlay g)
  , ToJSON (GamePlay g)
  , FromJSON (GamePlay g)
  , Eq (GameEnd g)
  , Show (GameEnd g)
  , ToJSON (GameEnd g)
  , FromJSON (GameEnd g)
  , Eq (GameState g)
  , Show (GameState g)
  , ToJSON (GameState g)
  , FromJSON (GameState g)
  ) =>
  Game g
  where
  -- | The type of game state associated with g
  type GameState g :: Type

  -- | The type of game play associated with g
  type GamePlay g :: Type

  -- | The situation at end of game
  type GameEnd g :: Type

-- | A handle to some underlying server for a single Head.
data Server g c m = Server
  { initHead :: [Text] -> m HeadId
  -- ^ Initialises a head with given parties.
  -- Might throw an exception if something goes wrong before hitting the
  -- underlying chain.
  , commit :: Integer -> HeadId -> m ()
  -- ^ Commit some value to the given head.
  -- The server is responsible for finding a suitable `Coin` that will fit the
  -- amount funded.
  -- Might throw a `ServerException` if something goes wrong.
  , play :: HeadId -> Int -> m ()
  -- ^ When the game is opened, do one play identified by its index in the list
  -- of `possibleActions`.
  -- Might throw a `ServerException` if the play is invalid.
  , newGame :: HeadId -> m ()
  -- ^ When the game has ended, restarts a new one with initial state and possible
  -- plays.
  -- Might throw a `ServerException` if the play is invalid.
  , closeHead :: HeadId -> m ()
  -- ^ Close the given head, effectively stopping the game and committing back
  -- payoffs on-chain. Fanout will be posted automatically at end of contestation
  -- period.
  -- Might throw a `ServerException` if the play is invalid.
  , poll :: Integer -> Integer -> m (Indexed g c)
  -- ^ Poll server for latest `FromChain` messages available.
  -- Takes the first event to retrieve and the maximum number of elements to send back.
  -- It will return 0 or more messages, depending on what's available, and the index
  -- of the last known message.
  }

data FromChain g c
  = HeadCreated {headId :: HeadId, parties :: [Party c]}
  | FundCommitted {headId :: HeadId, party :: Party c, coin :: Coin c}
  | HeadOpened {headId :: HeadId}
  | GameStarted {headId :: HeadId, game :: GameState g, plays :: [GamePlay g]}
  | GameChanged {headId :: HeadId, game :: GameState g, plays :: [GamePlay g]}
  | GameEnded {headId :: HeadId, gameEnd :: GameEnd g}
  | HeadClosed {headId :: HeadId}
  deriving (Generic)

deriving instance (Game g, IsChain c) => Show (FromChain g c)
deriving instance (Game g, IsChain c) => Eq (FromChain g c)
deriving instance (Game g, IsChain c) => ToJSON (FromChain g c)
deriving instance (Game g, IsChain c) => FromJSON (FromChain g c)

data Indexed g c = Indexed {lastIndex :: Integer, events :: [FromChain g c]}
  deriving (Generic)

deriving instance (Game g, IsChain c) => Show (Indexed g c)
deriving instance (Game g, IsChain c) => Eq (Indexed g c)
deriving instance (Game g, IsChain c) => ToJSON (Indexed g c)
deriving instance (Game g, IsChain c) => FromJSON (Indexed g c)

data Host = Host {host :: Text, port :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
