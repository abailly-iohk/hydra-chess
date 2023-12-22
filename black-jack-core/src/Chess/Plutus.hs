{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Chess.Plutus where

import PlutusTx.Prelude

import Cardano.Api (
  PlutusScriptVersion (PlutusScriptV2),
  Script (..),
  hashScript,
  serialiseToRawBytes,
  serialiseToTextEnvelope,
 )
import qualified Cardano.Api as Api
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import Data.ByteString.Short (ShortByteString)
import Data.String (IsString (..))
import PlutusLedgerApi.V2 (
  ScriptHash (..),
  SerialisedScript,
  UnsafeFromData,
 )
import PlutusTx (UnsafeFromData (..))
import qualified PlutusTx

-- | Signature of an untyped validator script.
type ValidatorType = BuiltinData -> BuiltinData -> BuiltinData -> ()

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

-- | Encodes a compiled `PlutusScriptV2` validator into a representation suitable for cardano-cli.
--
-- This function is intended to be used to provide the representation
-- of a script to be stored in a file and passed as
-- `--tx-out-script-file` or `--mint-script-file` to the
-- `cardano-cli`.
validatorToBytes :: ShortByteString -> ByteString
validatorToBytes =
  Lazy.toStrict
    . Aeson.encode
    . serialiseToTextEnvelope (Just $ fromString "Chess Token Policy")
    . PlutusScriptSerialised @Api.PlutusScriptV2
