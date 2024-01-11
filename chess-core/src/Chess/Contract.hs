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

import Chess.Game (Game (..), Move, Side (..), apply)
import Chess.GameState (ChessGame (..), ChessPlay (..))
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
  txInfoSignatories,
  txOutDatum,
 )
import PlutusLedgerApi.V2.Contexts (findDatumHash, getContinuingOutputs)
import PlutusTx (CompiledCode)
import qualified PlutusTx

validator :: ChessGame -> ChessPlay -> ScriptContext -> Bool
validator chess play scriptContext =
  case play of
    ChessMove move -> checkMove move chess scriptContext
    End -> checkGameEnd chess scriptContext
{-# INLINEABLE validator #-}

checkMove :: Move -> ChessGame -> ScriptContext -> Bool
checkMove move chess@ChessGame{players, game} scriptContext@ScriptContext{scriptContextTxInfo = txInfo} =
  isPlayersTurn game
    && case apply move game of
      Left{} -> traceError "Illegal move"
      Right game' -> checkGameOutput scriptContext chess{game = game'}
 where
  isPlayersTurn Game{curSide}
    | length players == 2 = checkPlayerTurn curSide
    | length players == 1 = True -- solo mode
    | otherwise = traceError "Number of players must be 1 or 2"

  checkPlayerTurn side =
    case txInfoSignatories txInfo of
      [signer] ->
        case findIndex (== signer) players of
          Just idx ->
            traceIfFalse "Wrong side to play" $
              (idx == 0 && side == White) || (idx == 1 && side == Black)
          Nothing -> traceError "Wrong signer"
      [] ->
        traceError "No signers"
      _ ->
        traceError "Too many signers"
{-# INLINEABLE checkMove #-}

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
