{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Game.ClientSpec where

import Chess.Generators ()
import Control.Concurrent.Class.MonadSTM (
  MonadSTM,
  atomically,
  modifyTVar',
  newTVarIO,
  readTVar,
  readTVarIO,
  writeTVar,
 )
import Control.Monad.Class.MonadAsync (MonadAsync (race_))
import Control.Monad.Class.MonadTimer (threadDelay)
import Control.Monad.IOSim (runSimOrThrow)
import Data.String (IsString)
import Game.Chess (Chess)
import Game.Client (runClient)
import Game.Client.IO (Command (..), Err (..), HasIO (..), Output (..))
import Game.Server (
  FromChain (..),
  Host (Host),
  Indexed (Indexed),
  IsChain (..),
  Server (..),
 )
import Game.Server.Mock (MockChain, MockCoin, MockParty (..))
import Test.Aeson.GenericSpecs (Proxy (Proxy), roundtripAndGoldenSpecs)
import Test.Hspec (Spec, describe, expectationFailure)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (
  Arbitrary (..),
  Property,
  elements,
  forAll,
  getNonEmpty,
  sublistOf,
 )

spec :: Spec
spec = do
  describe "Serialisation" $ do
    roundtripAndGoldenSpecs (Proxy @(Indexed Chess MockChain))

  describe "Fund Table" $ do
    prop "commit to head some funds given table created" prop_commit_to_head_when_funding_table

withIOSimIO :: forall m a. (MonadSTM m) => [Command] -> (HasIO Command Output m -> m a) -> m (a, [Output])
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
          , problem = \o -> atomically $ modifyTVar' outputs (o :)
          , prompt = pure ()
          , exit = pure ()
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
          let server = connectedServer{poll = \_ _ -> pure $ Indexed 0 [HeadCreated @Chess @MockChain mockId peers]}
          withIOSimIO [NewTable (pid <$> peers), FundTable mockId value] $ runClient server (const $ pure ())
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

mockId :: (IsString a) => a
mockId = "1234"

connectedServer :: (Monad m) => Server Chess MockChain m
connectedServer =
  Server
    { initHead = \_ -> pure mockId
    , commit = \_ _ -> pure ()
    , poll = \_ _ -> pure $ Indexed 0 []
    , play = \_ _ -> pure ()
    , closeHead = \_ -> pure ()
    , newGame = \_ -> pure ()
    }
