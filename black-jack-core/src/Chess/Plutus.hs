{-# LANGUAGE OverloadedStrings #-}
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
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised), fromPlutusData, toPlutusData)
import Cardano.Binary (serialize')
import Cardano.Crypto.Hash (Hash, hashToBytes)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy as Lazy
import Data.ByteString.Short (ShortByteString)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import PlutusLedgerApi.V2 (
  PubKeyHash (..),
  ScriptHash (..),
  SerialisedScript,
  ToData,
  UnsafeFromData,
  fromData,
  toData,
 )
import PlutusTx (UnsafeFromData (..))
import qualified PlutusTx
import qualified Prelude

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

pubKeyHashToHex :: PubKeyHash -> Text
pubKeyHashToHex (PubKeyHash bibs) =
  Text.decodeUtf8 $ Hex.encode $ fromBuiltin bibs

pubKeyHashFromHex :: Text -> PubKeyHash
pubKeyHashFromHex hex = PubKeyHash (toBuiltin bytes)
 where
  bytes :: ByteString
  bytes = case Hex.decode $ Text.encodeUtf8 hex of
    Left err -> Prelude.error $ "Fail to decode bytestring from hex " <> Text.unpack hex <> ": " <> err
    Right v -> v

fromJSONDatum :: (ToData a, PlutusTx.FromData a) => Aeson.Value -> Either Text a
fromJSONDatum value = do
  plutusData <-
    toPlutusData . Api.getScriptData
      <$> first (Text.pack . Prelude.show) (Api.scriptDataFromJson Api.ScriptDataJsonDetailedSchema value)
  maybe
    ( Left $
        Text.concat
          [ "Cannot convert "
          , Text.decodeUtf8 $ LBS.toStrict $ Aeson.encode value
          , " to plutus data"
          ]
    )
    Right
    $ fromData plutusData

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
