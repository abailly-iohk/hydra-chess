{-# LANGUAGE OverloadedStrings #-}

module BlackJack.Client.IOSpec where

import BlackJack.Client (Client (..), runClient)
import BlackJack.Client.IO (Command (..), Output (..), withInput)
import BlackJack.Server (HeadId)
import BlackJack.Server.Mock (MockChain)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = it "Quit exits from input loop" $ do
  let client = mockClient
  withInput [Quit] (runClient client) `shouldBe` ((), [Bye])

mockClient :: Monad m => Client MockChain m
mockClient =
  Client
    { newTable = const $ pure mockHeadId
    , fundTable = \_ _ -> pure ()
    , notify = pure []
    }

mockHeadId :: HeadId
mockHeadId = "1234"
