{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Chess.GoldenSpec where

import Chess.Generators ()
import Test.Aeson.GenericSpecs (Proxy (Proxy), roundtripAndGoldenSpecs)
import Test.Hspec (Spec)
import Chess.Game (Game)

spec :: Spec
spec = do
  roundtripAndGoldenSpecs (Proxy @Game)
