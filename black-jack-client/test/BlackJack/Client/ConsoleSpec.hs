{-# LANGUAGE OverloadedStrings #-}

module BlackJack.Client.ConsoleSpec where

import BlackJack.Client.Console (readInput)
import BlackJack.Client.IO (Command (..))
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec =
  it "parses 'quit' command" $ do
    readInput "quit" `shouldBe` Right Quit
    readInput "q" `shouldBe` Right Quit
