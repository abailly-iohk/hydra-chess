{-# LANGUAGE OverloadedStrings #-}

module Games.Server.HydraSpec where

import Data.Aeson (object, (.=))
import Test.Hspec (Spec, it, shouldBe)
import Game.Client.Console (parseQueryUTxO)

spec :: Spec
spec =
  it "can parse UTxO from cardano-cli to Hydra API Request" $ do
    let rawOutput = "5dd0cd84e86525c8d8264928b9d9a0c238d6dd07b6a919618b6d393234559df4     0        1200000 lovelace + 1 e18ad836532a69a93160efe11bcfac05b812a092ef3420042e700c10.666f6f + TxOutDatumNone"
        expected =
          object
            [ "5dd0cd84e86525c8d8264928b9d9a0c238d6dd07b6a919618b6d393234559df4#0"
                .= object
                  [ "lovelace" .= (1200000 :: Int)
                  , "e18ad836532a69a93160efe11bcfac05b812a092ef3420042e700c10"
                      .= object
                        ["666f6f" .= (1 :: Int)]
                  ]
            ]

    parseQueryUTxO rawOutput `shouldBe` Right expected
