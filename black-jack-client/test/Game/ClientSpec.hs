{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Game.ClientSpec where

import Control.Concurrent.Class.MonadSTM (MonadSTM, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Monad.Class.MonadAsync (MonadAsync (race_))
import Control.Monad.Class.MonadTimer (threadDelay)
import Control.Monad.IOSim (runSimOrThrow)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import Data.String (IsString)
import Data.Text (pack)
import Game.BlackJack (BlackJack)
import Game.Client (runClient)
import Game.Client.IO (Command (..), Err (..), HasIO (..), Output (..))
import Game.Server (FromChain (..), Host (Host), Indexed (Indexed, events), IsChain (..), Server (..))
import Game.Server.Mock (MockChain, MockCoin, MockParty (..))
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (
  Arbitrary (..),
  Property,
  elements,
  forAll,
  getNonEmpty,
  sublistOf,
 )
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs, Proxy (Proxy))

sampleEvents :: ByteString
sampleEvents =
  "{\"events\":[{\"headId\":\"5c9d0a7d44fa71ea4745a1ea62a930dc\",\"parties\":[{\"host\":{\"host\":\"localhost\",\"port\":12345},\"pid\":\"alice\"},{\"host\":{\"host\":\"localhost\",\"port\":12346},\"pid\":\"bob\"}],\"tag\":\"HeadCreated\"},"
    <> "{\"coin\":100,\"headId\":\"5c9d0a7d44fa71ea4745a1ea62a930dc\",\"party\":{\"host\":{\"host\":\"localhost\",\"port\":12345},\"pid\":\"alice\"},\"tag\":\"FundCommitted\"}"
    <> ",{\"coin\":100,\"headId\":\"5c9d0a7d44fa71ea4745a1ea62a930dc\",\"party\":{\"host\":{\"host\":\"localhost\",\"port\":12346},\"pid\":\"bob\"},\"tag\":\"FundCommitted\"},"
    <> "{\"headId\":\"5c9d0a7d44fa71ea4745a1ea62a930dc\",\"tag\":\"HeadOpened\"},"
    <> "{\"game\":{\"initialBets\":[],\"numPlayers\":1,\"tag\":\"Setup\"},\"headId\":\"5c9d0a7d44fa71ea4745a1ea62a930dc\",\"plays\":[{\"contents\":{\"playerId\":1,\"tag\":\"PlayerId\"},\"tag\":\"Bet\"}],\"tag\":\"GameStarted\"},"
    <> "{\"game\":{\"dealerHand\":[{\"color\":\"♠\",\"face\":\"7\"}],\"next\":{\"playerId\":1,\"tag\":\"PlayerId\"},\"numPlayers\":1,\"players\":[{\"bets\":100,\"hand\":[{\"color\":\"♠\",\"face\":\"K\"},{\"color\":\"♢\",\"face\":\"K\"}],\"tag\":\"Playing\"}],\"tag\":\"BlackJack\"},\"headId\":\"5c9d0a7d44fa71ea4745a1ea62a930dc\",\"plays\":[{\"contents\":{\"playerId\":1,\"tag\":\"PlayerId\"},\"tag\":\"Hit\"},{\"contents\":{\"playerId\":1,\"tag\":\"PlayerId\"},\"tag\":\"Stand\"}],\"tag\":\"GameChanged\"}],\"lastIndex\":6}"

spec :: Spec
spec = do
  describe "Serialisation" $ do
    roundtripAndGoldenSpecs (Proxy @(Indexed BlackJack MockChain))

    it "can deserialise events list" $ do
      let parsed = eitherDecode sampleEvents

      (length . events @BlackJack @MockChain <$> parsed) `shouldBe` Right 2

  describe "New Table" $ do
    it "is notified when invited to a new table" $
      failAfter 10 $ do
        let res = runSimOrThrow $ do
              let peers = [alice, bob]
                  server = connectedServer{poll = \_ _ -> pure $ Indexed 1 [HeadCreated @BlackJack @MockChain mockId peers]}
              withIOSimIO [] $ runClient server
        res `shouldBe` ((), [Ok (pack $ show $ HeadCreated @BlackJack @MockChain mockId [alice, bob])])
  describe "Fund Table" $ do
    prop "commit to head some funds given table created" prop_commit_to_head_when_funding_table

withIOSimIO :: forall m a. MonadSTM m => [Command] -> (HasIO m -> m a) -> m (a, [Output])
withIOSimIO cmds act = do
  inputs <- newTVarIO cmds
  outputs <- newTVarIO []
  let io =
        HasIO
          { input =
              atomically $
                readTVar inputs >>= \case
                  [] -> pure $ Left EOF
                  (t : ts) -> writeTVar inputs ts >> pure (Right t)
          , output = \o -> atomically $ modifyTVar' outputs (o :)
          , prompt = pure ()
          }
  res <- act io
  outs <- reverse <$> readTVarIO outputs
  pure (res, outs)

failAfter :: Int -> IO () -> IO ()
failAfter duration =
  race_
    (threadDelay (fromIntegral duration) >> expectationFailure ("Timeout fired after " <> show duration <> "s"))

prop_commit_to_head_when_funding_table :: KnownParties -> Committable -> Property
prop_commit_to_head_when_funding_table (KnownParties peers) (Committable coins) =
  forAll (elements coins) $ \c ->
    let value = coinValue @MockChain c
        result = runSimOrThrow $ do
          let server = connectedServer{poll = \_ _ -> pure $ Indexed 0 [HeadCreated @BlackJack @MockChain mockId peers]}
          withIOSimIO [NewTable (pid <$> peers), FundTable mockId value] $ runClient server
     in Ok "committed" `elem` snd result

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

connectedServer :: Monad m => Server BlackJack MockChain m
connectedServer =
  Server
    { initHead = \_ -> pure mockId
    , commit = \_ _ -> pure ()
    , poll = \_ _ -> pure $ Indexed 0 []
    , play = \_ _ -> pure ()
    , closeHead = \_ -> pure ()
    , newGame = \_ -> pure ()
    }
