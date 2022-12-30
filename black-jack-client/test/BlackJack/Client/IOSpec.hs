{-# LANGUAGE OverloadedStrings #-}

module BlackJack.Client.IOSpec where

import BlackJack.Client (runClient)
import BlackJack.Client.IO (Command (..), Output (..), mkPureIO, withInput)
import BlackJack.ClientSpec (connectedServer)
import Data.Functor.Identity (runIdentity)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = it "Quit exits from input loop" $ do
  runIdentity (withInput [Quit] (runClient connectedServer mkPureIO)) `shouldBe` ((), [Bye])
