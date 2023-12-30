{-# LANGUAGE OverloadedStrings #-}

module Games.Server.HydraSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Game.Client.Console (Coin (..), Coins (..), SimpleUTxO (..), parseQueryUTxO)
import Games.Server.Hydra (extractGameToken, extractGameState)
import Test.Hspec (Spec, it, shouldBe)
import qualified Data.ByteString.Lazy as LBS
import qualified Chess.Game as Chess

spec :: Spec
spec = do
  it "can parse UTxO from cardano-cli to Hydra API Request" $ do
    let rawOutput = "5dd0cd84e86525c8d8264928b9d9a0c238d6dd07b6a919618b6d393234559df4     0        1200000 lovelace + 1 e18ad836532a69a93160efe11bcfac05b812a092ef3420042e700c10.666f6f + TxOutDatumNone"
        expected =
          SimpleUTxO
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

  it "can parse UTxO without any datum from cardano-cli to Hydra API Request" $ do
    let rawOutput = "5dd0cd84e86525c8d8264928b9d9a0c238d6dd07b6a919618b6d393234559df4     0        1200000 lovelace + 1 e18ad836532a69a93160efe11bcfac05b812a092ef3420042e700c10.666f6f"
        expected =
          SimpleUTxO
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

  it "can parse UTxO with datum hash from cardano-cli to Hydra API Request" $ do
    let rawOutput = "c5a00b09e82c334bd04d62313ab25608ca70e7d1014ca9c8dfc09251d51ea6a0     0        10000000 lovelace + 1 e18ad836532a69a93160efe11bcfac05b812a092ef3420042e700c10.1ad7cb51c9e2d6250bd80395a5c920f6f628adc4b1bd057a81c9be98 + TxOutDatumHash ScriptDataInBabbageEra \"36643c8dbde0ad0f092aec2d4d672730e863d6f8d034c7da3b8c31d868e20b4e\""
        expected =
          UTxOWithDatum
            { txIn = "c5a00b09e82c334bd04d62313ab25608ca70e7d1014ca9c8dfc09251d51ea6a0#0"
            , coins =
                Coins 10000000 $
                  Map.fromList
                    [
                      ( "e18ad836532a69a93160efe11bcfac05b812a092ef3420042e700c10"
                      , Coin (Map.fromList [("1ad7cb51c9e2d6250bd80395a5c920f6f628adc4b1bd057a81c9be98", 1)])
                      )
                    ]
            , datumhash = "36643c8dbde0ad0f092aec2d4d672730e863d6f8d034c7da3b8c31d868e20b4e"
            }

    parseQueryUTxO rawOutput `shouldBe` Right expected

  it "can parse UTxO with datum hash from cardano-cli to Hydra API Request" $ do
    let rawOutput = "c5a00b09e82c334bd04d62313ab25608ca70e7d1014ca9c8dfc09251d51ea6a0     0        10000000 lovelace + 1 e18ad836532a69a93160efe11bcfac05b812a092ef3420042e700c10.1ad7cb51c9e2d6250bd80395a5c920f6f628adc4b1bd057a81c9be98 + TxOutDatumHash AlonzoEraOnwardsBabbage \"36643c8dbde0ad0f092aec2d4d672730e863d6f8d034c7da3b8c31d868e20b4e\""
        expected =
          UTxOWithDatum
            { txIn = "c5a00b09e82c334bd04d62313ab25608ca70e7d1014ca9c8dfc09251d51ea6a0#0"
            , coins =
                Coins 10000000 $
                  Map.fromList
                    [
                      ( "e18ad836532a69a93160efe11bcfac05b812a092ef3420042e700c10"
                      , Coin (Map.fromList [("1ad7cb51c9e2d6250bd80395a5c920f6f628adc4b1bd057a81c9be98", 1)])
                      )
                    ]
            , datumhash = "36643c8dbde0ad0f092aec2d4d672730e863d6f8d034c7da3b8c31d868e20b4e"
            }

    parseQueryUTxO rawOutput `shouldBe` Right expected

  it "extracts game token from JSON UTxO" $ do
    let utxo = fromJust $ Aeson.decode "{\"c5a00b09e82c334bd04d62313ab25608ca70e7d1014ca9c8dfc09251d51ea6a0#0\":{\"address\":\"addr_test1wz4y5mkg3m83dh3npqygnzst74s26cewjw3uel2ylcuqagg9zad83\",\"datum\":null,\"datumhash\":\"36643c8dbde0ad0f092aec2d4d672730e863d6f8d034c7da3b8c31d868e20b4e\",\"inlineDatum\":null,\"referenceScript\":null,\"value\":{\"e18ad836532a69a93160efe11bcfac05b812a092ef3420042e700c10\":{\"1ad7cb51c9e2d6250bd80395a5c920f6f628adc4b1bd057a81c9be98\":1},\"lovelace\":10000000}}}"

    extractGameToken "e18ad836532a69a93160efe11bcfac05b812a092ef3420042e700c10" "1ad7cb51c9e2d6250bd80395a5c920f6f628adc4b1bd057a81c9be98" utxo
      `shouldBe` Just "c5a00b09e82c334bd04d62313ab25608ca70e7d1014ca9c8dfc09251d51ea6a0#0"

  it "extracts game state from UTxO" $ do
    let utxo = fromJust $ Aeson.decode snapshotConfirmed
    extractGameState "addr_test1wrr66kuw94l4zh7jff227872cyppk2ttketmdsqcflwhwhchp6lwz" utxo
      `shouldBe` Right Chess.initialGame

snapshotConfirmed :: LBS.ByteString
snapshotConfirmed = "{\"6fcb7389fb0ae8ae12c3b119801933e923edcf702bc4da1c7ee2bbda1c30f9b9#0\":{\"address\":\"addr_test1wrr66kuw94l4zh7jff227872cyppk2ttketmdsqcflwhwhchp6lwz\",\"datum\":null,\"inlineDatum\":{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"list\":[{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":1},{\"int\":0}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":1},{\"int\":1}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":1},{\"int\":2}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":1},{\"int\":3}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":1},{\"int\":4}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":1},{\"int\":5}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":1},{\"int\":6}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":1},{\"int\":7}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":6},{\"int\":0}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":6},{\"int\":1}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":6},{\"int\":2}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":6},{\"int\":3}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":6},{\"int\":4}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":6},{\"int\":5}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":6},{\"int\":6}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":0,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":6},{\"int\":7}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":1,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":7},{\"int\":0}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":1,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":7},{\"int\":7}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":0},{\"int\":0}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":0},{\"int\":7}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":3,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":7},{\"int\":1}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":3,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":7},{\"int\":6}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":3,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":0},{\"int\":1}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":3,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":0},{\"int\":6}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":2,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":7},{\"int\":2}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":2,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":7},{\"int\":5}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":2,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":0},{\"int\":2}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":2,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":0},{\"int\":5}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":4,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":7},{\"int\":3}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":4,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":0},{\"int\":3}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":5,\"fields\":[]},{\"constructor\":1,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":7},{\"int\":4}]}]},{\"constructor\":0,\"fields\":[{\"constructor\":5,\"fields\":[]},{\"constructor\":0,\"fields\":[]},{\"constructor\":0,\"fields\":[{\"int\":0},{\"int\":4}]}]}]}]},\"inlineDatumhash\":\"934b1342da03dc0d86326c02c733c8c6e8e3c46847a04c47e3326e2fc0fedc24\",\"referenceScript\":null,\"value\":{\"e18ad836532a69a93160efe11bcfac05b812a092ef3420042e700c10\":{\"1ad7cb51c9e2d6250bd80395a5c920f6f628adc4b1bd057a81c9be98\":1},\"lovelace\":2000000}},\"6fcb7389fb0ae8ae12c3b119801933e923edcf702bc4da1c7ee2bbda1c30f9b9#1\":{\"address\":\"addr_test1vqdd0j63e83dvfgtmqpetfwfyrm0v29dcjcm6pt6s8ymaxqr3z3f6\",\"datum\":null,\"datumhash\":null,\"inlineDatum\":null,\"referenceScript\":null,\"value\":{\"lovelace\":8000000}}}"
