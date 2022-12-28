{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module BlackJack.ClientSpec where

import BlackJack.Client (Client (..), Result (..), startClient)
import BlackJack.Server (InitResult (..), IsParty (..), Server (..))
import Control.Monad.IOSim (runSimOrThrow)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..), Positive (..), Property, forAll, generate, sublistOf, suchThat)

spec :: Spec
spec = do
  describe "New Table" $ do
    prop "initialises head when new table requested" prop_init_head_on_new_table
    it "returns table creation error given initialisation fails" $ do
      KnownParties parties <- generate arbitrary

      let result = runSimOrThrow $ do
            let failingServer = (connectedServer parties){initHead = const $ pure (pure $ InitFailed "fail to init")}
            Client{newTable} <- startClient failingServer
            newTable (fst <$> parties)

      result `shouldBe` TableCreationFailed "fail to init"

  describe "Fund Table" $
    prop "commit to head some funds given table created" prop_commit_to_head_when_funding_table

prop_init_head_on_new_table :: KnownParties -> Property
prop_init_head_on_new_table (KnownParties parties) =
  forAll (sublistOf parties) $ \peers ->
    let result = runSimOrThrow $ do
          Client{newTable} <- startClient (connectedServer parties)
          newTable (fst <$> peers)
     in result == TableCreated (snd <$> peers) mockId

prop_commit_to_head_when_funding_table :: KnownParties -> Positive Integer -> Property
prop_commit_to_head_when_funding_table (KnownParties parties) (Positive availableFunds) =
  forAll (arbitrary `suchThat` (< availableFunds)) $ \funding ->
    let result = runSimOrThrow $ do
          Client{newTable, fundTable} <- startClient (connectedServer parties)
          TableCreated{tableId} <- newTable (fst <$> parties)
          fundTable tableId funding
     in result == TableFunded funding mockId

newtype KnownParties = KnownParties [(Text, MockParty)]
  deriving newtype (Eq, Show)

instance Arbitrary KnownParties where
  arbitrary =
    KnownParties
      <$> sublistOf
        ( (\n -> (n, Party n))
            <$> ["bob", "carol", "daniel", "emily", "francis"]
        )

newtype MockParty = Party Text
  deriving newtype (Eq, Show)

instance IsParty MockParty where
  partyId (Party s) = s

mockId :: Text
mockId = "1234"

connectedServer :: Monad m => [(Text, MockParty)] -> Server MockParty m
connectedServer parties =
  Server
    { connect = pure . fromJust . flip lookup parties
    , initHead = const $ pure (pure $ InitDone mockId)
    }
