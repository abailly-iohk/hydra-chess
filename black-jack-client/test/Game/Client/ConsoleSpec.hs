{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Client.ConsoleSpec where

import Game.Client.Console (readInput, inputParser)
import Game.Client.IO (Command (..))
import Game.ClientSpec (KnownParties (KnownParties))
import Game.Server (HeadId (HeadId), partyId)
import Game.Server.Mock (MockChain)
import qualified Data.Text as Text
import Test.Hspec (Spec, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Small (Small), (===), tabulate)
import Test.QuickCheck.Modifiers (Positive (Positive))
import Data.Aeson (encode)
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy as LT
import Data.Function ((&))

spec :: Spec
spec = do
  it "parses 'quit' command" $ do
    readInput inputParser "quit" `shouldBe` Right Quit
    readInput inputParser "q" `shouldBe` Right Quit

  prop "parses 'newTable' command" $ \(KnownParties parties) ->
    let names = (partyId @MockChain <$> parties)
     in readInput inputParser ("newTable " <> Text.unwords names) `shouldBe` Right (NewTable names)

  prop "parses 'fundTable' command" $ \(HeadId headId) (Positive (Small n)) ->
    readInput inputParser ("fundTable " <> headId <> " " <> Text.pack (show n)) `shouldBe` Right (FundTable headId n)

  prop "parses 'play' command" $ \(HeadId headId) json ->
    readInput inputParser ("play " <> headId <> " " <> LT.toStrict (LT.decodeUtf8 $ encode json)) === Right (Play headId json)
     & tabulate "Values" [ head $ words $ show json ]

  prop "parses 'newGame' command" $ \(HeadId headId) ->
    readInput inputParser ("newGame " <> headId) `shouldBe` Right (NewGame headId)

  prop "parses 'stop' command" $ \(HeadId headId) ->
    readInput inputParser ("stop " <> headId) `shouldBe` Right (Stop headId)
