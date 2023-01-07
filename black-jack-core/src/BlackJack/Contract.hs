{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialize -fdefer-type-errors #-}

module BlackJack.Contract where

import Plutus.V2.Ledger.Api (
  Script,
  ScriptContext (..),
  UnsafeFromData,
  Validator (getValidator),
  mkValidatorScript,
  unsafeFromBuiltinData,
 )
import PlutusTx (CompiledCode)
import qualified PlutusTx
import PlutusTx.Prelude

type DatumType = ()
type RedeemerType = ()

validator :: DatumType -> RedeemerType -> ScriptContext -> Bool
validator _game _play _scriptContext = True

-- | Signature of an untyped validator script.
type ValidatorType = BuiltinData -> BuiltinData -> BuiltinData -> ()

compiledValidator :: CompiledCode ValidatorType
compiledValidator =
  $$(PlutusTx.compile [||wrap validator||])
 where
  wrap = wrapValidator @DatumType @RedeemerType

validatorScript :: Script
validatorScript = getValidator $ mkValidatorScript compiledValidator

wrapValidator ::
  (UnsafeFromData datum, UnsafeFromData redeemer, UnsafeFromData context) =>
  (datum -> redeemer -> context -> Bool) ->
  ValidatorType
wrapValidator f d r c =
  check $ f datum redeemer context
 where
  datum = unsafeFromBuiltinData d
  redeemer = unsafeFromBuiltinData r
  context = unsafeFromBuiltinData c
{-# INLINEABLE wrapValidator #-}
