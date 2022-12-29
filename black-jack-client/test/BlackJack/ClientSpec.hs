{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module BlackJack.ClientSpec where

import BlackJack.Client (Client (..), Result (..), asText, startClient)
import BlackJack.Server (CommitResult (..), FromChain (..), Host (Host), InitResult (..), IsChain (..), Server (..))
import BlackJack.Server.Mock (MockChain, MockCoin (MockCoin), MockParty (..))
import Control.Monad.Class.MonadAsync (MonadAsync (race_), concurrently)
import Control.Monad.Class.MonadTimer (threadDelay)
import Control.Monad.IOSim (runSimOrThrow)
import Data.Function ((&))
import Data.Text (Text)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
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

    it "is notified when invited to a new table" $
      failAfter 10 $ do
        let res = runSimOrThrow $ do
              let peers = [alice, bob]
                  server = (connectedServer [] peers){poll = pure $ Just $ HeadCreated mockId peers}
                  waitForSomething notif = do
                    notif >>= \case
                      Nothing -> threadDelay 1 >> waitForSomething notif
                      Just r -> pure r
                  client1 = do
                    Client{newTable} <- startClient server
                    newTable ["bob"]
                  client2 = do
                    Client{notify} <- startClient server
                    waitForSomething notify
              concurrently client1 client2
        res `shouldBe` (TableCreated [bob] mockId, TableCreated [alice, bob] mockId)

  describe "Fund Table" $ do
    prop "commit to head some funds given table created" prop_commit_to_head_when_funding_table
    prop "commit fails when funds do not match existing coins" prop_commit_fail_on_non_matching_coins

failAfter :: Int -> IO () -> IO ()
failAfter duration =
  race_
    (threadDelay (fromIntegral duration) >> expectationFailure ("Timeout fired after " <> show duration <> "s"))

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
    let value = coinValue @MockChain coin
        result = runSimOrThrow $ do
          Client{newTable, fundTable} <- startClient (connectedServer coins parties)
          TableCreated{tableId} <- newTable (partyId @MockChain <$> parties)
          fundTable tableId value
     in result === TableFunded coin mockId

prop_commit_fail_on_non_matching_coins :: KnownParties -> Property
prop_commit_fail_on_non_matching_coins (KnownParties parties) =
  forAll arbitrary $ \(getPositive -> value) ->
    let expectedError = NoMatchingCoin value []
        result = runSimOrThrow $ do
          Client{newTable, fundTable} <- startClient ((connectedServer [] parties){commit = const $ const $ pure $ pure expectedError})
          TableCreated{tableId} <- newTable (partyId @MockChain <$> parties)
          fundTable tableId value
     in result === TableFundingFailed (asText expectedError)

newtype KnownParties = KnownParties [MockParty]
  deriving newtype (Eq, Show)

instance Arbitrary KnownParties where
  arbitrary =
    KnownParties
      <$> sublistOf [bob, carol, daniel, emily, francis]

alice, bob, carol, daniel, emily, francis :: MockParty
alice = Party{host = Host "1.2.3.4" 1234, pid = "alice"}
bob = Party{host = Host "1.2.3.5" 2345, pid = "bob"}
carol = Party{host = Host "1.2.3.6" 2345, pid = "carol"}
daniel = Party{host = Host "1.2.3.7" 2345, pid = "daniel"}
emily = Party{host = Host "1.2.3.8" 2345, pid = "emily"}
francis = Party{host = Host "1.2.3.9" 2345, pid = "francis"}

newtype Committable = Committable [MockCoin]
  deriving newtype (Eq, Show)

instance Arbitrary Committable where
  arbitrary = Committable . getNonEmpty <$> arbitrary
  shrink (Committable coins) = Committable <$> filter (not . null) (shrink coins)

mockId :: Text
mockId = "1234"

connectedServer :: Monad m => [MockCoin] -> [MockParty] -> Server MockChain m
connectedServer _coins parties =
  Server
    { initHead = \ps -> pure (pure $ InitDone mockId $ filter (\p -> partyId @MockChain p `elem` ps) parties)
    , commit = \value _ -> pure (pure $ CommitDone $ MockCoin value)
    , poll = pure $ Just $ HeadCreated mockId parties
    }
