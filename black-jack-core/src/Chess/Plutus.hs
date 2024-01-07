{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Chess.Plutus (module Chess.Plutus, ToData)
where

import PlutusTx.Prelude

import Cardano.Binary (serialize')
import Cardano.Crypto.Hash (Blake2b_224, Hash, hashToBytes, hashWith)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Lazy as Lazy
import Data.ByteString.Short (ShortByteString, fromShort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import PlutusLedgerApi.V2 (
  PubKeyHash (..),
  ScriptHash (..),
  SerialisedScript,
  ToData,
  UnsafeFromData,
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
    . hashToBytes
    . hashWith @Blake2b_224
      ( \sbs ->
          "\x02" -- the babbageScriptPrefixTag defined in cardano-ledger for PlutusV2 scripts
            Prelude.<> fromShort sbs
      )

-- | Encodes a compiled `PlutusScriptV2` validator into a representation suitable for cardano-cli.
--
-- This function is intended to be used to provide the representation
-- of a script to be stored in a file and passed as
-- `--tx-out-script-file` or `--mint-script-file` to the
-- `cardano-cli`.
validatorToBytes :: ShortByteString -> ByteString
validatorToBytes script =
  Lazy.toStrict
    . Aeson.encode
    $ object
      [ "type" .= ("PlutusScriptV2" :: Text)
      , "description" .= ("Chess Script" :: Text)
      , "cborHex" .= Text.decodeUtf8 (Hex.encode $ serialize' script)
      ]

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
