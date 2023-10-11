{-# LANGUAGE TypeApplications #-}

module BlackJack.Contract.GameSpec where

import BlackJack.Contract.Game (BlackJack (..), Card, Color, DealerHand)
import BlackJack.Game ()
import Test.Aeson.GenericSpecs (Proxy (Proxy), roundtripAndGoldenSpecs)
import Test.Hspec (Spec)

spec :: Spec
spec = do
  roundtripAndGoldenSpecs (Proxy @Color)
  roundtripAndGoldenSpecs (Proxy @Card)
  roundtripAndGoldenSpecs (Proxy @DealerHand)
  roundtripAndGoldenSpecs (Proxy @BlackJack)
