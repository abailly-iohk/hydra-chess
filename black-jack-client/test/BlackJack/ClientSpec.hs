{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module BlackJack.ClientSpec where

import BlackJack.Client (Client (..), runClient, startClient)
import BlackJack.Client.IO (Output (Ok), mkPureIO, withInput)
import BlackJack.Server (FromChain (..), Host (Host), IsChain (..), Server (..), ServerException (ServerException))
import BlackJack.Server.Mock (MockChain, MockCoin, MockParty (..))
import Control.Monad.Class.MonadAsync (MonadAsync (race_))
import Control.Monad.Class.MonadThrow (MonadThrow (throwIO), evaluate)
import Control.Monad.Class.MonadTimer (threadDelay)
import Control.Monad.IOSim (runSimOrThrow)
import Data.Function ((&))
import Data.String (IsString)
import Test.Hspec (Spec, anyException, describe, expectationFailure, it, shouldBe, shouldThrow)
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
            let failingServer = connectedServer{initHead = const $ pure mockId}
            Client{newTable} <- startClient failingServer
            newTable (partyId @MockChain <$> parties)

      result `shouldBe` mockId

    it "is notified when invited to a new table" $
      failAfter 10 $ do
        let res = runSimOrThrow $ do
              let peers = [alice, bob]
                  server = connectedServer{poll = pure [HeadCreated @MockChain mockId peers]}
                  client2 = withInput [] $ runClient server mkPureIO
              client2
        res `shouldBe` ((), [Ok ""])
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
          Client{newTable} <- startClient connectedServer
          newTable (partyId @MockChain <$> peers)
     in result == mockId
          & counterexample ("Result: " <> show result)
          & counterexample ("Peers: " <> show peers)

prop_commit_to_head_when_funding_table :: KnownParties -> Committable -> Property
prop_commit_to_head_when_funding_table (KnownParties parties) (Committable coins) =
  forAll (elements coins) $ \coin ->
    let value = coinValue @MockChain coin
        result = runSimOrThrow $ do
          Client{newTable, fundTable} <- startClient connectedServer
          tableId <- newTable (partyId @MockChain <$> parties)
          fundTable tableId value
     in result === ()

prop_commit_fail_on_non_matching_coins :: KnownParties -> Property
prop_commit_fail_on_non_matching_coins (KnownParties parties) =
  forAll arbitrary $ \(getPositive -> value) ->
    let result = runSimOrThrow $ do
          Client{newTable, fundTable} <- startClient (connectedServer{commit = \_ _ -> throwIO (ServerException "error")})
          tableId <- newTable (partyId @MockChain <$> parties)
          fundTable tableId value
     in evaluate result `shouldThrow` anyException

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

mockId :: IsString a => a
mockId = "1234"

connectedServer :: Monad m => Server MockChain m
connectedServer =
  Server
    { initHead = \_ -> pure mockId
    , commit = \_ _ -> pure ()
    , poll = pure []
    }
