{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module BlackJack.ClientSpec where

import BlackJack.Client (Client (..), Result (..), startClient)
import BlackJack.Server (InitResult (..), IsParty (..), Server (..))
import Control.Exception (evaluate)
import Control.Monad.IOSim (runSimOrThrow)
import Data.Maybe (fromJust)
import Test.Hspec (Spec, anyException, it, shouldThrow)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..), Property, forAll, generate, sublistOf)

spec :: Spec
spec = do
  prop "initialises head when new table requested" prop_init_head_on_new_table
  it "throws exception when initialisation fails" $ do
    KnownParties parties <- generate arbitrary

    let result = runSimOrThrow $ do
          let failingServer = (connectedServer parties){initHead = const $ pure (pure $ InitFailed "fail to init")}
          Client{newTable} <- startClient failingServer
          newTable (fst <$> parties)

    evaluate result `shouldThrow` anyException

prop_init_head_on_new_table :: KnownParties -> Property
prop_init_head_on_new_table (KnownParties parties) =
  forAll (sublistOf parties) $ \peers ->
    let result = runSimOrThrow $ do
          Client{newTable} <- startClient (connectedServer parties)
          newTable (fst <$> peers)
     in result == TableCreated (snd <$> peers)

newtype KnownParties = KnownParties [(String, MockParty)]
  deriving newtype (Eq, Show)

instance Arbitrary KnownParties where
  arbitrary =
    KnownParties
      <$> sublistOf
        ( (\n -> (n, Party n))
            <$> ["bob", "carol", "daniel", "emily", "francis"]
        )

newtype MockParty = Party String
  deriving newtype (Eq, Show)

instance IsParty MockParty where
  partyId (Party s) = s

connectedServer :: Monad m => [(String, MockParty)] -> Server MockParty m
connectedServer parties =
  Server
    { connect = pure . fromJust . flip lookup parties
    , initHead = const $ pure (pure InitDone)
    }
