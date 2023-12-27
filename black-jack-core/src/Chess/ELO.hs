{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialize -fdefer-type-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

-- | A smart contract that controls a player's ELO rating and their
-- participation in games within Hydra.
--
-- The contract is parameterised by the player's public key so that it
-- can only be committed to a head by the controlling player.  It can
-- also be spent to start a new game, in which case it becomes locked
-- until the game ends.
module Chess.ELO where

import PlutusTx.Prelude

import Cardano.Api (hashScriptDataBytes, serialiseToRawBytes, unsafeHashableScriptData)
import Cardano.Api.Shelley (fromPlutusData, scriptDataToJson)
import Cardano.Binary (serialize')
import Chess.Plutus (ValidatorType, scriptValidatorHash, validatorToBytes)
import Data.ByteString (ByteString)
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V2 (PubKeyHash, ScriptHash, SerialisedScript, serialiseCompiledCode, ToData)
import PlutusTx (CompiledCode, compile, liftCode, toData, unsafeApplyCode)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as Lazy
import Cardano.Api.Shelley (ScriptDataJsonSchema(ScriptDataJsonDetailedSchema))

-- FIXME: check script can only be spent by `PubKeyHash`
validator :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> Bool
validator _pkh _ _ _ = True
{-# INLINEABLE validator #-}

compiledValidator :: CompiledCode (PubKeyHash -> ValidatorType)
compiledValidator =
  $$(compile [||(\pkh d r c -> check $ validator pkh d r c)||])

validatorScript :: PubKeyHash -> SerialisedScript
validatorScript pkh =
  serialiseCompiledCode $
    compiledValidator
      `unsafeApplyCode` PlutusTx.liftCode plcVersion100 pkh

validatorHash :: PubKeyHash -> ScriptHash
validatorHash = scriptValidatorHash . validatorScript

validatorBytes :: PubKeyHash -> ByteString
validatorBytes = validatorToBytes . validatorScript

datumHashBytes :: Integer -> ByteString
datumHashBytes =
  serialiseToRawBytes
    . hashScriptDataBytes
    . unsafeHashableScriptData
    . fromPlutusData
    . toData

datumBytes :: ToData a => a -> ByteString
datumBytes =
  serialize'
    . fromPlutusData
    . toData

datumJSON :: ToData a => a -> ByteString
datumJSON =
  Lazy.toStrict
    . Aeson.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . unsafeHashableScriptData
    . fromPlutusData
    . toData
