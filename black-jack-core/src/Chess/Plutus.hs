{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Chess.Plutus where

import PlutusTx.Prelude

import Cardano.Api (
  PlutusScriptVersion (PlutusScriptV2),
  Script (..),
  hashScript,
  serialiseToRawBytes,
 )
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised))
import PlutusLedgerApi.V2 (
  ScriptHash (..),
  SerialisedScript,
  UnsafeFromData,
 )
import PlutusTx (UnsafeFromData (..))
import qualified PlutusTx

-- | Signature of an untyped minting policy script.
type MintingPolicyType = BuiltinData -> BuiltinData -> ()

data MintAction = Mint | Burn

PlutusTx.unstableMakeIsData ''MintAction

wrapMintingPolicy ::
  (UnsafeFromData redeemer, UnsafeFromData context) =>
  (redeemer -> context -> Bool) ->
  MintingPolicyType
wrapMintingPolicy f r c =
  check $ f redeemer context
 where
  redeemer = unsafeFromBuiltinData r
  context = unsafeFromBuiltinData c
{-# INLINEABLE wrapMintingPolicy #-}

-- * Similar utilities as plutus-ledger

-- | Compute the on-chain 'ScriptHash' for a given serialised plutus script. Use
-- this to refer to another validator script.
scriptValidatorHash :: SerialisedScript -> ScriptHash
scriptValidatorHash =
  ScriptHash
    . toBuiltin
    . serialiseToRawBytes
    . hashScript
    . PlutusScript PlutusScriptV2
    . PlutusScriptSerialised
