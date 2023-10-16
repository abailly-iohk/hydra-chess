{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Games.Run.Hydra where

import Cardano.Binary (serialize')
import Cardano.Crypto.DSIGN (Ed25519DSIGN, genKeyDSIGN, seedSizeDSIGN)
import Cardano.Crypto.Seed (readSeedFromSystemEntropy)
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import Control.Monad (unless)
import Control.Monad.Class.MonadAsync (race)
import Data.Aeson (Value (Number), eitherDecode, encode, object, (.=))
import Data.Aeson.KeyMap (KeyMap, insert, (!?))
import Data.Aeson.Types (Value (Object))
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Lazy as LBS
import Data.Data (Proxy (..))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as Lazy
import Game.Server (Host (..))
import Games.Run.Cardano (CardanoNode (..), checkProcessHasNotDied, findCardanoCliExecutable, findSocketPath, withLogFile)
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest)
import System.Directory (Permissions (..), XdgDirectory (..), createDirectoryIfMissing, doesFileExist, getPermissions, getXdgDirectory, setOwnerExecutable, setPermissions)
import System.FilePath ((</>))
import System.Process (CreateProcess (..), StdStream (..), proc, readProcess, withCreateProcess)

data HydraNode = HydraNode {hydraHost :: Host}
  deriving (Show)

withHydraNode :: CardanoNode -> (HydraNode -> IO a) -> IO a
withHydraNode CardanoNode{nodeSocket} k =
  withLogFile "hydra-node" $ \out -> do
    exe <- findHydraExecutable
    process <- hydraNodeProcess exe nodeSocket
    withCreateProcess process{std_out = UseHandle out, std_err = UseHandle out} $
      \_stdin _stdout _stderr processHandle ->
        race
          (checkProcessHasNotDied "hydra-node" processHandle)
          (k (HydraNode (Host "localhost" 34567)))
          >>= \case
            Left{} -> error "should never been reached"
            Right a -> pure a

hydraScriptsTxId :: String
hydraScriptsTxId = "4793d318ec98741c0eebff7c62af6389a860c6e51c4fa1961cc5b7eab5a46f58"

hydraNodeProcess :: FilePath -> FilePath -> IO CreateProcess
hydraNodeProcess executableFile nodeSocket = do
  hydraSkFile <- findHydraSigningKey
  cardanoSkFile <- findCardanoSigningKey
  protocolParametersFile <- findProtocolParametersFile
  hydraPersistenceDir <- findHydraPersistenceDir
  let
    nodeId = "hydra"
    hydraPort :: Int = 5551
    monitoringPort :: Int = 6001
    args =
      [ "--node-id"
      , nodeId
      , "--api-host"
      , "127.0.0.1"
      , "--host"
      , "0.0.0.0"
      , "--port"
      , show hydraPort
      , "--monitoring-port"
      , show monitoringPort
      , "--persistence-dir"
      , hydraPersistenceDir
      , "--hydra-signing-key"
      , hydraSkFile
      , "--cardano-signing-key"
      , cardanoSkFile
      , "--ledger-protocol-parameters"
      , protocolParametersFile
      , "--testnet-magic"
      , "2"
      , "--hydra-scripts-tx-id"
      , hydraScriptsTxId
      , "--node-socket"
      , nodeSocket
      ]
  pure $ proc executableFile args

ed2559seedsize :: Word
ed2559seedsize = seedSizeDSIGN (Proxy @Ed25519DSIGN)

findHydraSigningKey :: IO FilePath
findHydraSigningKey = do
  configDir <- getXdgDirectory XdgConfig "hydra-node"
  createDirectoryIfMissing True configDir
  let hydraSk = configDir </> "hydra.sk"
  exists <- doesFileExist hydraSk
  unless exists $ do
    seed <- readSeedFromSystemEntropy ed2559seedsize
    let sk = genKeyDSIGN @Ed25519DSIGN seed
        jsonEnvelope =
          object
            [ "type" .= ("HydraSigningKey_ed25519" :: Text)
            , "description" .= ("" :: Text)
            , "cborHex" .= decodeUtf8 (Hex.encode (serialize' sk))
            ]

    LBS.writeFile hydraSk (encode jsonEnvelope)
  pure hydraSk

findCardanoSigningKey :: IO FilePath
findCardanoSigningKey = do
  configDir <- getXdgDirectory XdgConfig "hydra-node"
  createDirectoryIfMissing True configDir
  let cardanoSk = configDir </> "cardano.sk"
  exists <- doesFileExist cardanoSk
  unless exists $ do
    seed <- readSeedFromSystemEntropy (seedSizeDSIGN (Proxy @Ed25519DSIGN))
    let sk = genKeyDSIGN @Ed25519DSIGN seed
        jsonEnvelope =
          object
            [ "type" .= ("PaymentSigningKeyShelley_ed25519" :: Text)
            , "description" .= ("Payment Signing Key" :: Text)
            , "cborHex" .= decodeUtf8 (Hex.encode (serialize' sk))
            ]

    LBS.writeFile cardanoSk (encode jsonEnvelope)
  pure cardanoSk

findHydraPersistenceDir :: IO FilePath
findHydraPersistenceDir = do
  persistenceDir <- getXdgDirectory XdgCache "hydra-node"
  createDirectoryIfMissing True persistenceDir
  pure persistenceDir

findProtocolParametersFile :: IO FilePath
findProtocolParametersFile = do
  configDir <- getXdgDirectory XdgConfig "hydra-node"
  createDirectoryIfMissing True configDir
  let hydraSk = configDir </> "protocol-parameters.json"
  exists <- doesFileExist hydraSk
  unless exists $ do
    cardanoCliExe <- findCardanoCliExecutable
    socketPath <- findSocketPath
    out <-
      eitherDecode . Lazy.encodeUtf8 . LT.pack
        <$> readProcess cardanoCliExe ["query", "protocol-parameters", "--testnet-magic", "2", "--socket-path", socketPath] ""
    either
      (\err -> error $ "Failed to extract protocol parameters: " <> show err)
      (LBS.writeFile hydraSk . encode)
      (mkZeroFeeParams <$> out)
  pure hydraSk

mkZeroFeeParams :: Value -> Value
mkZeroFeeParams = \case
  Object obj ->
    Object $
      insert "txFeeFixed" zero $
        insert "txFeePerByte" zero $
          updateExecutionPrices obj
  other -> other
 where
  zero = Number 0

  updateExecutionPrices :: KeyMap Value -> KeyMap Value
  updateExecutionPrices m =
    case m !? "executionUnitPrices" of
      Just (Object obj) ->
        insert
          "executionUnitPrices"
          ( Object $
              insert "pricesMemory" zero $
                insert "pricesSteps" zero $
                  obj
          )
          m
      _ -> m

findHydraExecutable :: IO FilePath
findHydraExecutable = do
  dataDir <- getXdgDirectory XdgData "hydra"
  createDirectoryIfMissing True dataDir
  let hydraExecutable = dataDir </> "hydra-node"
  exists <- doesFileExist hydraExecutable
  unless exists $ downloadHydraExecutable dataDir
  permissions <- getPermissions hydraExecutable
  unless (executable permissions) $ setPermissions hydraExecutable (setOwnerExecutable True permissions)
  pure hydraExecutable

downloadHydraExecutable :: FilePath -> IO ()
downloadHydraExecutable destDir = do
  let binariesUrl = "https://github.com/input-output-hk/hydra/releases/download/0.12.0/tutorial-binaries-aarch64-darwin.tar.gz"
  request <- parseRequest $ "GET " <> binariesUrl
  putStr "Downloading hydra executables"
  httpLBS request >>= Tar.unpack destDir . Tar.read . GZip.decompress . getResponseBody
  putStrLn " done"
