{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Games.Run.Hydra where

import Cardano.Binary (fromCBOR, serialize')
import Cardano.Crypto.DSIGN (
  DSIGNAlgorithm (SignKeyDSIGN),
  Ed25519DSIGN,
  VerKeyDSIGN,
  deriveVerKeyDSIGN,
  genKeyDSIGN,
  seedSizeDSIGN,
 )
import Cardano.Crypto.Seed (readSeedFromSystemEntropy)
import qualified Codec.Archive.Tar as Tar
import Codec.CBOR.Read (deserialiseFromBytes)
import qualified Codec.Compression.GZip as GZip
import Control.Monad (unless, when)
import Control.Monad.Class.MonadAsync (race)
import Data.Aeson (Value (Number, String), eitherDecode, encode, object, (.=))
import Data.Aeson.KeyMap (KeyMap, insert, (!?))
import Data.Aeson.Types (Value (Object))
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Lazy as LBS
import Data.Data (Proxy (..))
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as Lazy
import Game.Server (Host (..))
import Games.Run.Cardano (
  CardanoNode (..),
  Network (..),
  checkProcessHasNotDied,
  findCardanoCliExecutable,
  findSocketPath,
  networkDir,
  networkMagicArgs,
  withLogFile,
 )
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest)
import System.Directory (
  Permissions (..),
  XdgDirectory (..),
  createDirectoryIfMissing,
  doesFileExist,
  getPermissions,
  getXdgDirectory,
  setOwnerExecutable,
  setPermissions,
 )
import System.FilePath ((</>))
import System.Process (
  CreateProcess (..),
  StdStream (..),
  callProcess,
  proc,
  readProcess,
  withCreateProcess,
 )

data HydraNode = HydraNode
  { hydraParty :: VerKeyDSIGN Ed25519DSIGN
  , hydraHost :: Host
  }
  deriving (Show)

withHydraNode :: CardanoNode -> (HydraNode -> IO a) -> IO a
withHydraNode CardanoNode{network, nodeSocket} k =
  withLogFile "hydra-node" $ \out -> do
    exe <- findHydraExecutable
    (me, process) <- hydraNodeProcess network exe nodeSocket
    withCreateProcess process{std_out = UseHandle out, std_err = UseHandle out} $
      \_stdin _stdout _stderr processHandle ->
        race
          (checkProcessHasNotDied "hydra-node" processHandle)
          (k (HydraNode me (Host "127.0.0.1" 34567)))
          >>= \case
            Left{} -> error "should never been reached"
            Right a -> pure a

findHydraScriptsTxId :: Network -> IO String
findHydraScriptsTxId = \case
  -- TODO: those should be pulled from some remotely published source
  Preview -> pure "1e00c627ec4b2ad0b4aa68068d3818ca0e41338c87e5504cda118c4050a98763"
  Preprod -> pure "f917dcd1fa2653e33d6d0ca5a067468595b546120c3085fab60848c34f92c265"
  Mainnet -> pure "989e3ab136a2cdd3132a99975e76e02f62bcb03ba64ddbb5d2dfddffca8d390d"

hydraNodeProcess :: Network -> FilePath -> FilePath -> IO (VerKeyDSIGN Ed25519DSIGN, CreateProcess)
hydraNodeProcess network executableFile nodeSocket = do
  (me, hydraSkFile) <- findHydraSigningKey network
  cardanoSkFile <- findCardanoSigningKey network
  cardanoVkFile <- findCardanoVerificationKey network
  checkFundsAreAvailable network cardanoSkFile cardanoVkFile
  protocolParametersFile <- findProtocolParametersFile network
  hydraPersistenceDir <- findHydraPersistenceDir network
  hydraScriptsTxId <- findHydraScriptsTxId network
  let
    nodeId = "hydra"
    hydraPort :: Int = 5551
    apiPort :: Int = 34567
    monitoringPort :: Int = 6001
    args =
      ( [ "--node-id"
        , nodeId
        , "--api-host"
        , "127.0.0.1"
        , "--api-port"
        , show apiPort
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
        , "--hydra-scripts-tx-id"
        , hydraScriptsTxId
        , "--node-socket"
        , nodeSocket
        ]
          <> networkMagicArgs network
      )
  pure (me, proc executableFile args)

checkFundsAreAvailable :: Network -> FilePath -> FilePath -> IO ()
checkFundsAreAvailable network signingKeyFile verificationKeyFile = do
  cardanoCliExe <- findCardanoCliExecutable
  socketPath <- findSocketPath network
  ownAddress <- readProcess cardanoCliExe (["address", "build", "--verification-key-file", verificationKeyFile] <> networkMagicArgs network) ""
  output <-
    drop 2 . lines
      <$> readProcess
        cardanoCliExe
        ( [ "query"
          , "utxo"
          , "--address"
          , ownAddress
          , "--socket-path"
          , socketPath
          ]
            <> networkMagicArgs network
        )
        ""
  when (length output < 2) $
    putStrLn $
      "Hydra needs some funds to fuel the process, please send at least 2 UTxOs with over 10 ADAs each to " <> ownAddress

ed2559seedsize :: Word
ed2559seedsize = seedSizeDSIGN (Proxy @Ed25519DSIGN)

findHydraSigningKey :: Network -> IO (VerKeyDSIGN Ed25519DSIGN, FilePath)
findHydraSigningKey network = do
  configDir <- getXdgDirectory XdgConfig ("hydra-node" </> networkDir network)
  createDirectoryIfMissing True configDir
  let hydraSk = configDir </> "hydra.sk"
  exists <- doesFileExist hydraSk
  if not exists
    then do
      seed <- readSeedFromSystemEntropy ed2559seedsize
      let sk = genKeyDSIGN @Ed25519DSIGN seed
          jsonEnvelope =
            object
              [ "type" .= ("HydraSigningKey_ed25519" :: Text)
              , "description" .= ("" :: Text)
              , "cborHex" .= decodeUtf8 (Hex.encode (serialize' sk))
              ]

      LBS.writeFile hydraSk (encode jsonEnvelope)
      pure (deriveVerKeyDSIGN sk, hydraSk)
    else do
      envelope <- eitherDecode <$> LBS.readFile hydraSk
      let sk = case envelope of
            Right (Object val) -> do
              case val !? "cborHex" of
                Just (String str) ->
                  case Hex.decode (encodeUtf8 str) of
                    Right bs ->
                      either
                        (\err -> error $ "Failed to deserialised signing key " <> show bs <> " : " <> show err)
                        snd
                        $ deserialiseFromBytes @(SignKeyDSIGN Ed25519DSIGN) fromCBOR (LBS.fromStrict bs)
                    Left err -> error $ "Failed to deserialised signing key " <> unpack str <> " : " <> err
                other -> error $ "Failed to deserialised signing key " <> show other
            other -> error $ "Failed to read Hydra signing key file " <> hydraSk <> ", " <> show other
      pure (deriveVerKeyDSIGN sk, hydraSk)

findCardanoSigningKey :: Network -> IO FilePath
findCardanoSigningKey network = do
  configDir <- getXdgDirectory XdgConfig ("hydra-node" </> networkDir network)
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

findCardanoVerificationKey :: Network -> IO FilePath
findCardanoVerificationKey network = do
  configDir <- getXdgDirectory XdgConfig ("hydra-node" </> networkDir network)
  createDirectoryIfMissing True configDir
  let cardanoVk = configDir </> "cardano.vk"
  exists <- doesFileExist cardanoVk
  unless exists $ do
    cardanoSk <- findCardanoSigningKey network
    cardanoCliExe <- findCardanoCliExecutable
    callProcess cardanoCliExe ["key", "verification-key", "--signing-key-file", cardanoSk, "--verification-key-file", cardanoVk]
  pure cardanoVk

findHydraPersistenceDir :: Network -> IO FilePath
findHydraPersistenceDir network = do
  persistenceDir <- getXdgDirectory XdgCache ("hydra-node" </> networkDir network)
  createDirectoryIfMissing True persistenceDir
  pure persistenceDir

findProtocolParametersFile :: Network -> IO FilePath
findProtocolParametersFile network = do
  configDir <- getXdgDirectory XdgConfig ("hydra-node" </> networkDir network)
  createDirectoryIfMissing True configDir
  let hydraSk = configDir </> "protocol-parameters.json"
  exists <- doesFileExist hydraSk
  unless exists $ do
    cardanoCliExe <- findCardanoCliExecutable
    socketPath <- findSocketPath network
    out <-
      eitherDecode . Lazy.encodeUtf8 . LT.pack
        <$> readProcess cardanoCliExe (["query", "protocol-parameters", "--socket-path", socketPath] <> networkMagicArgs network) ""
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
                insert "pricesSteps" zero obj
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
  -- TODO: generalise URL when binaries are published
  let binariesUrl = "https://github.com/input-output-hk/hydra/releases/download/0.12.0/tutorial-binaries-aarch64-darwin.tar.gz"
  request <- parseRequest $ "GET " <> binariesUrl
  putStr "Downloading hydra executables"
  httpLBS request >>= Tar.unpack destDir . Tar.read . GZip.decompress . getResponseBody
  putStrLn " done"
