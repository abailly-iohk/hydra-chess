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

import Chess.Game (apply)
import Chess.Plutus (ValidatorType, scriptValidatorHash, wrapValidator)
import PlutusLedgerApi.V2 (
  Datum (Datum),
  OutputDatum (..),
  ScriptContext (..),
  ScriptHash (..),
  SerialisedScript,
  getDatum,
  serialiseCompiledCode,
  toBuiltinData,
  txOutDatum,
 )
import PlutusLedgerApi.V2.Contexts (findDatumHash, getContinuingOutputs)
import PlutusTx (CompiledCode)
import qualified PlutusTx
import Chess.GameState (ChessGame (..), ChessPlay (..))

validator :: ChessGame -> ChessPlay -> ScriptContext -> Bool
validator chess@ChessGame{game} play scriptContext =
  case play of
    ChessMove move ->  case apply move game of
      Left{} -> traceError "Illegal move"
      Right game' -> checkGameOutput scriptContext chess { game = game' }
    End -> checkGameEnd chess scriptContext
{-# INLINEABLE validator #-}

checkGameOutput :: ScriptContext -> ChessGame -> Bool
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

  expectedHash = findDatumHash (Datum expectedData) txInfo

  ownDatum =
    case getContinuingOutputs ctx of
      [o] -> txOutDatum o
      _ -> traceError "expected only one head output"

  ScriptContext{scriptContextTxInfo = txInfo} = ctx
{-# INLINEABLE checkGameOutput #-}

-- | Verifies game is ending correctly and players get rewarded accordingly.
-- TODO
checkGameEnd :: ChessGame -> ScriptContext -> Bool
checkGameEnd _ _ = True
{-# INLINEABLE checkGameEnd #-}

compiledValidator :: CompiledCode ValidatorType
compiledValidator =
  $$(PlutusTx.compile [||wrap validator||])
 where
  wrap = wrapValidator @ChessGame @ChessPlay

validatorScript :: SerialisedScript
validatorScript = serialiseCompiledCode compiledValidator

validatorHash :: ScriptHash
validatorHash = scriptValidatorHash validatorScript
