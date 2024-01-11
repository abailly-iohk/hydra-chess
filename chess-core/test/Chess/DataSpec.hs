{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

module Chess.DataSpec where

import Chess.Data (PlutusData)
import Test.Aeson.GenericSpecs (Proxy (Proxy), roundtripAndGoldenSpecs)
import Test.Hspec (Spec)

spec :: Spec
spec =
  roundtripAndGoldenSpecs (Proxy @PlutusData)
