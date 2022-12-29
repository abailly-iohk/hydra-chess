{-# LANGUAGE OverloadedStrings #-}

module BlackJack.Client.IOSpec where

import BlackJack.Client (Client (..), Result (..), runClient)
import BlackJack.Client.IO (Command (..), Output (..), withInput)
import BlackJack.Server.Mock (MockChain)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = it "Quit exits from input loop" $ do
  let client = mockClient
  withInput [Quit] (runClient client) `shouldBe` ((), [Bye])

mockClient :: Monad m => Client MockChain m
mockClient =
  Client
    { newTable = const $ pure (TableCreated [] "1234")
    , fundTable = \_ _ -> pure (TableFunded mempty "1234")
    , notify = pure Nothing
    }
