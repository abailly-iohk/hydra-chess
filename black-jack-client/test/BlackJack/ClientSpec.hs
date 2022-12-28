{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module BlackJack.ClientSpec where

import BlackJack.Client (Client (..), Result (..), startClient)
import BlackJack.Server (CommitResult (CommitDone), InitResult (..), IsChain (..), Server (..))
import Control.Monad.IOSim (runSimOrThrow)
import Data.Function ((&))
import Data.Maybe (fromJust)
import Data.Monoid (Sum (..))
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (
  Arbitrary (..),
  Positive (..),
  Property,
  counterexample,
  elements,
  forAll,
  generate,
  getNonEmpty,
  sublistOf,
  (===),
 )

spec :: Spec
spec = do
  describe "New Table" $ do
    prop "initialises head when new table requested" prop_init_head_on_new_table
    it "returns table creation error given initialisation fails" $ do
      KnownParties parties <- generate arbitrary

      let result = runSimOrThrow $ do
            let failingServer = (connectedServer [] parties){initHead = const $ pure (pure $ InitFailed "fail to init")}
            Client{newTable} <- startClient failingServer
            newTable (partyId @MockChain <$> parties)

      result `shouldBe` TableCreationFailed "fail to init"

  describe "Fund Table" $
    prop "commit to head some funds given table created" prop_commit_to_head_when_funding_table

prop_init_head_on_new_table :: KnownParties -> Property
prop_init_head_on_new_table (KnownParties parties) =
  forAll (sublistOf parties) $ \peers ->
    let result = runSimOrThrow $ do
          Client{newTable} <- startClient (connectedServer [] parties)
          newTable (partyId @MockChain <$> peers)
     in result == TableCreated peers mockId
          & counterexample ("Result: " <> show result)
          & counterexample ("Peers: " <> show peers)

prop_commit_to_head_when_funding_table :: KnownParties -> Committable -> Property
prop_commit_to_head_when_funding_table (KnownParties parties) (Committable coins) =
  forAll (elements coins) $ \coin ->
    let result = runSimOrThrow $ do
          Client{newTable, fundTable} <- startClient (connectedServer coins parties)
          TableCreated{tableId} <- newTable (partyId @MockChain <$> parties)
          fundTable tableId (coinValue @MockChain coin)
     in result === TableFunded coin mockId

newtype KnownParties = KnownParties [MockParty]
  deriving newtype (Eq, Show)

instance Arbitrary KnownParties where
  arbitrary =
    KnownParties
      <$> sublistOf
        (Party <$> ["bob", "carol", "daniel", "emily", "francis"])

newtype Committable = Committable [MockCoin]
  deriving newtype (Eq, Show)

instance Arbitrary Committable where
  arbitrary = Committable . getNonEmpty <$> arbitrary
  shrink (Committable coins) = Committable <$> filter (not . null) (shrink coins)

data MockChain = MockChain

newtype MockCoin = MockCoin Integer
  deriving newtype (Eq, Show)
  deriving (Semigroup, Monoid) via Sum Integer

instance Arbitrary MockCoin where
  arbitrary = MockCoin . getPositive <$> arbitrary
  shrink (MockCoin c) = MockCoin <$> shrink c

newtype MockParty = Party Text
  deriving newtype (Eq, Show)

instance IsChain MockChain where
  type Party MockChain = MockParty
  type Coin MockChain = MockCoin

  partyId (Party s) = s

  coinValue (MockCoin c) = c

mockId :: Text
mockId = "1234"

connectedServer :: Monad m => [MockCoin] -> [MockParty] -> Server MockChain m
connectedServer _coins parties =
  let partyMap = (\p -> (partyId @MockChain p, p)) <$> parties
   in Server
        { connect = pure . fromJust . flip lookup partyMap
        , initHead = const $ pure (pure $ InitDone mockId)
        , commit = \value -> pure (pure $ CommitDone $ MockCoin value)
        }
