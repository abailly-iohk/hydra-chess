{-# LANGUAGE TypeApplications #-}

module BlackJack.Contract.GameSpec where

import BlackJack.Contract.Game (Color, Card, DealerHand)
import BlackJack.Game (BlackJack)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs, Proxy (Proxy))
import Test.Hspec (Spec)

spec :: Spec
spec = do
  roundtripAndGoldenSpecs (Proxy @Color)
  roundtripAndGoldenSpecs (Proxy @Card)
  roundtripAndGoldenSpecs (Proxy @DealerHand)
  roundtripAndGoldenSpecs (Proxy @BlackJack)
