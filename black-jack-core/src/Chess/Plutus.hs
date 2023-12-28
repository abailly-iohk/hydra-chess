{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Chess.Plutus (module Chess.Plutus, ToData)
where

import PlutusTx.Prelude

import Cardano.Api (
  PlutusScriptVersion (PlutusScriptV2),
  Script (..),
  hashScript,
  serialiseToRawBytes,
  serialiseToTextEnvelope,
 )
import qualified Cardano.Api as Api
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised), fromPlutusData)
import Cardano.Binary (serialize')
import Cardano.Crypto.Hash (Hash, hashToBytes)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import Data.ByteString.Short (ShortByteString)
import Data.String (IsString (..))
import PlutusLedgerApi.V2 (
  PubKeyHash (..),
  ScriptHash (..),
  SerialisedScript,
  ToData,
  UnsafeFromData,
  toData,
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
    . serialiseToTextEnvelope (Just $ fromString "Chess Script")
    . PlutusScriptSerialised @Api.PlutusScriptV2

pubKeyHash :: Hash h keyRole -> PubKeyHash
pubKeyHash h = PubKeyHash (toBuiltin @ByteString $ hashToBytes h)

datumHashBytes :: (ToData a) => a -> ByteString
datumHashBytes =
  serialiseToRawBytes
    . Api.hashScriptDataBytes
    . Api.unsafeHashableScriptData
    . fromPlutusData
    . toData

datumBytes :: (ToData a) => a -> ByteString
datumBytes =
  serialize'
    . fromPlutusData
    . toData

datumJSON :: (ToData a) => a -> ByteString
datumJSON =
  Lazy.toStrict
    . Aeson.encode
    . Api.scriptDataToJson Api.ScriptDataJsonDetailedSchema
    . Api.unsafeHashableScriptData
    . fromPlutusData
    . toData
