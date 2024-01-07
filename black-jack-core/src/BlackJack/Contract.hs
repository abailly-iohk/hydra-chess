{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialize -fdefer-type-errors #-}

module BlackJack.Contract where

import PlutusTx.Prelude

import BlackJack.Contract.Game (possibleActions)
import BlackJack.Game (BlackJack, Play)
import PlutusLedgerApi.V2 (
  Datum (Datum),
  OutputDatum (..),
  ScriptContext (..),
  ScriptHash (..),
  SerialisedScript,
  ToData,
  UnsafeFromData,
  getDatum,
  serialiseCompiledCode,
  toBuiltinData,
  txOutDatum,
  unsafeFromBuiltinData,
 )
import PlutusLedgerApi.V2.Contexts (findDatumHash, getContinuingOutputs)
import PlutusTx (CompiledCode)
import qualified PlutusTx
import Chess.Plutus (scriptValidatorHash)

type DatumType = BlackJack
type RedeemerType = Play

validator :: DatumType -> RedeemerType -> ScriptContext -> Bool
validator game play _scriptContext =
  play `elem` possibleActions game

checkHeadOutputDatum :: ToData a => ScriptContext -> a -> Bool
checkHeadOutputDatum ctx d =
  case ownDatum of
    NoOutputDatum ->
      traceError "missing datum"
    OutputDatumHash actualHash ->
      traceIfFalse
        "output datum hash mismatch"
        ( Just actualHash == expectedHash
        )
    OutputDatum actual ->
      traceIfFalse "output datum mismatch" $ getDatum actual == expectedData
 where
  expectedData = toBuiltinData d

  expectedHash = findDatumHash (Datum $ toBuiltinData d) txInfo

  ownDatum =
    case getContinuingOutputs ctx of
      [o] -> txOutDatum o
      _ -> traceError "expected only one head output"

  ScriptContext{scriptContextTxInfo = txInfo} = ctx
{-# INLINEABLE checkHeadOutputDatum #-}

-- | Signature of an untyped validator script.
type ValidatorType = BuiltinData -> BuiltinData -> BuiltinData -> ()

compiledValidator :: CompiledCode ValidatorType
compiledValidator =
  $$(PlutusTx.compile [||wrap validator||])
 where
  wrap = wrapValidator @DatumType @RedeemerType

validatorScript :: SerialisedScript
validatorScript = serialiseCompiledCode compiledValidator

validatorHash :: ScriptHash
validatorHash = scriptValidatorHash validatorScript

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
