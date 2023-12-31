{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialize -fdefer-type-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module Chess.Contract where

import PlutusTx.Prelude

import Chess.Game (Game, Move, apply)
import PlutusLedgerApi.V2 (
  Datum (Datum),
  OutputDatum (..),
  ScriptContext (..),
  ScriptHash (..),
  SerialisedScript,
  ToData,
  getDatum,
  serialiseCompiledCode,
  toBuiltinData,
  txOutDatum,
 )
import PlutusLedgerApi.V2.Contexts (findDatumHash, getContinuingOutputs)
import PlutusTx (CompiledCode)
import qualified PlutusTx
import Chess.Plutus (ValidatorType, wrapValidator, scriptValidatorHash)

type DatumType = Game
type RedeemerType = Move

validator :: DatumType -> RedeemerType -> ScriptContext -> Bool
validator game move scriptContext =
  case apply move game of
    Left{} -> traceError "Illegal move"
    Right game' -> checkGameOutput scriptContext game'
{-# INLINEABLE validator #-}

checkGameOutput :: ToData a => ScriptContext -> a -> Bool
checkGameOutput ctx d =
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
{-# INLINEABLE checkGameOutput #-}

compiledValidator :: CompiledCode ValidatorType
compiledValidator =
  $$(PlutusTx.compile [||wrap validator||])
 where
  wrap = wrapValidator @DatumType @RedeemerType

validatorScript :: SerialisedScript
validatorScript = serialiseCompiledCode compiledValidator

validatorHash :: ScriptHash
validatorHash = scriptValidatorHash validatorScript
