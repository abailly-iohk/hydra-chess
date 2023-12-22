{-# LANGUAGE OverloadedStrings #-}

module Games.Server.HydraSpec where

import qualified Data.Map as Map
import Game.Client.Console (Coin (..), Coins (..), SimpleTxOut (..), parseQueryUTxO)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec =
  it "can parse UTxO from cardano-cli to Hydra API Request" $ do
    let rawOutput = "5dd0cd84e86525c8d8264928b9d9a0c238d6dd07b6a919618b6d393234559df4     0        1200000 lovelace + 1 e18ad836532a69a93160efe11bcfac05b812a092ef3420042e700c10.666f6f + TxOutDatumNone"
        expected =
          SimpleTxOut
            { txIn = "5dd0cd84e86525c8d8264928b9d9a0c238d6dd07b6a919618b6d393234559df4#0"
            , coins =
                Coins 1200000 $
                  Map.fromList
                    [
                      ( "e18ad836532a69a93160efe11bcfac05b812a092ef3420042e700c10"
                      , Coin (Map.fromList [("666f6f", 1)])
                      )
                    ]
            }

    parseQueryUTxO rawOutput `shouldBe` Right expected
