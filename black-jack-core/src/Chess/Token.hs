{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialize -fdefer-type-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

-- | Defines the currency policy for tokens representing a player's game.
module Chess.Token where

import PlutusTx.Prelude

import Chess.Plutus (
  MintAction,
  MintingPolicyType,
  scriptValidatorHash,
  validatorToBytes,
  wrapMintingPolicy,
 )
import PlutusLedgerApi.V2 (
  ScriptContext (..),
  ScriptHash (..),
  SerialisedScript,
  serialiseCompiledCode,
  toData,
 )

import Cardano.Api (
  ScriptDataJsonSchema (..),
  scriptDataToJson,
  unsafeHashableScriptData,
 )
import Cardano.Api.Shelley (
  fromPlutusData,
 )
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import PlutusTx (CompiledCode)
import qualified PlutusTx
import Prelude (Show (show), String)

validator ::
  MintAction ->
  ScriptContext ->
  Bool
validator _action _scriptContext =
  True
{-# INLINEABLE validator #-}

compiledValidator :: CompiledCode MintingPolicyType
compiledValidator =
  $$(PlutusTx.compile [||wrapMintingPolicy validator||])

validatorScript :: SerialisedScript
validatorScript = serialiseCompiledCode compiledValidator

validatorHash :: ScriptHash
validatorHash = scriptValidatorHash validatorScript

validatorHashHex :: String
validatorHashHex = show validatorHash

validatorBytes :: ByteString
validatorBytes =
  validatorToBytes validatorScript

mintActionJSON :: MintAction -> ByteString
mintActionJSON =
  Lazy.toStrict
    . Aeson.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . unsafeHashableScriptData
    . fromPlutusData
    . toData
