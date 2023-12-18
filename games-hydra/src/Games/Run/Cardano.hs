{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Games.Run.Cardano where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import Control.Exception (finally)
import Control.Monad (unless, when, (>=>))
import Control.Monad.Class.MonadAsync (race)
import Control.Monad.Class.MonadTimer (threadDelay)
import Data.Aeson (Value (..), eitherDecode, encode)
import Data.Aeson.KeyMap ((!?))
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text, unpack)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as Lazy
import Data.Void (Void)
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest)
import System.Directory (
  Permissions (..),
  XdgDirectory (..),
  createDirectoryIfMissing,
  doesFileExist,
  getPermissions,
  getXdgDirectory,
  removeFile,
  setOwnerExecutable,
  setPermissions,
 )
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (BufferMode (..), Handle, IOMode (..), hSetBuffering, withFile)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), proc, readCreateProcess, readProcess, terminateProcess, waitForProcess, withCreateProcess)

data CardanoNode = CardanoNode
  { nodeSocket :: FilePath
  , network :: Network
  }
  deriving (Show)

data Network = Preview | Preprod | Mainnet
  deriving stock (Eq, Show, Read)

cardanoNodeVersion :: Network -> String
cardanoNodeVersion Preview = "8.7.1"
cardanoNodeVersion Preprod = "8.7.1"
cardanoNodeVersion Mainnet = "8.7.1"

withCardanoNode :: Network -> (CardanoNode -> IO a) -> IO a
withCardanoNode network k =
  withLogFile ("cardano-node" </> networkDir network) $ \out -> do
    exe <- findCardanoExecutable (cardanoNodeVersion network)
    socketPath <- findSocketPath network
    process <- cardanoNodeProcess exe network
    withCreateProcess process{std_out = UseHandle out, std_err = UseHandle out} $
      \_stdin _stdout _stderr processHandle ->
        ( race
            (checkProcessHasNotDied network "cardano-node" processHandle)
            (waitForNode socketPath k)
            >>= \case
              Left{} -> error "should never been reached"
              Right a -> pure a
        )
          `finally` (cleanupSocketFile socketPath >> terminateProcess processHandle)
 where
  waitForNode socketPath cont = do
    let rn = CardanoNode{nodeSocket = socketPath, network}
    waitForSocket rn
    waitForFullSync rn
    cont rn

  cleanupSocketFile socketPath = do
    exists <- doesFileExist socketPath
    when exists $ removeFile socketPath

findSocketPath :: Network -> IO FilePath
findSocketPath network = do
  socketDir <- getXdgDirectory XdgCache ("cardano-node" </> networkDir network)
  createDirectoryIfMissing True socketDir
  pure $ socketDir </> "node.socket"

findCardanoExecutable :: String -> IO FilePath
findCardanoExecutable version = do
  dataDir <- getXdgDirectory XdgData "cardano"
  createDirectoryIfMissing True dataDir
  let cardanoExecutable = dataDir </> "cardano-node"
  exists <- doesFileExist cardanoExecutable
  hasRightVersion <-
    if exists
      then (== version) <$> getVersion cardanoExecutable
      else pure True
  when (not exists || not hasRightVersion) $ do
    downloadCardanoExecutable version dataDir
    permissions <- getPermissions cardanoExecutable
    unless (executable permissions) $ setPermissions cardanoExecutable (setOwnerExecutable True permissions)
  pure cardanoExecutable

getVersion :: FilePath -> IO String
getVersion exe =
  readProcess exe ["--version"] "" >>= pure . takeWhile (/= ' ') . drop 13

findCardanoCliExecutable :: IO FilePath
findCardanoCliExecutable = do
  dataDir <- getXdgDirectory XdgData "cardano"
  let cardanoCliExecutable = dataDir </> "cardano-cli"
  permissions <- getPermissions cardanoCliExecutable
  unless (executable permissions) $ setPermissions cardanoCliExecutable (setOwnerExecutable True permissions)
  pure cardanoCliExecutable

downloadCardanoExecutable :: String -> FilePath -> IO ()
downloadCardanoExecutable version destDir = do
  let binariesUrl =
        "https://github.com/intersectMBO/cardano-node/releases/download/"
          <> version <> "-pre"
          <> "/cardano-node-"
          <> version
          <> "-macos.tar.gz"
  request <- parseRequest $ "GET " <> binariesUrl
  putStr $ "Downloading cardano executables from " <> binariesUrl
  httpLBS request >>= Tar.unpack destDir . Tar.read . GZip.decompress . getResponseBody
  putStrLn " done"

-- | Wait for the node socket file to become available.
waitForSocket :: CardanoNode -> IO ()
waitForSocket node@CardanoNode{nodeSocket} = do
  exists <- doesFileExist nodeSocket
  unless exists $ do
    putStrLn "Cardano node launching"
    threadDelay 1_000_000
    waitForSocket node

-- | Wait for the node to be fully synchronized.
waitForFullSync :: CardanoNode -> IO ()
waitForFullSync node = do
  tip <- queryPercentSync node
  unless (tip == 100.0) $ do
    putStrLn $ "Cardano node syncing: " <> show tip <> "%"
    threadDelay 10_000_000
    waitForFullSync node

queryPercentSync :: CardanoNode -> IO Double
queryPercentSync CardanoNode{network} = do
  cardanoCliExe <- findCardanoCliExecutable
  socketPath <- findSocketPath network
  out <-
    (eitherDecode >=> extractSyncPercent) . Lazy.encodeUtf8 . LT.pack
      <$> readProcess cardanoCliExe (["query", "tip", "--socket-path", socketPath] <> networkMagicArgs network) ""
  either error pure out

extractSyncPercent :: Value -> Either String Double
extractSyncPercent = \case
  Object obj ->
    case obj !? "syncProgress" of
      Just (String txt) -> pure $ (read $ unpack txt)
      _ -> Left "Did not find 'syncProgress' field"
  v -> Left $ "query returned something odd: " <> LT.unpack (Lazy.decodeUtf8 $ encode v)

findConfigFiles :: Network -> IO (FilePath, FilePath)
findConfigFiles network = do
  let nodeConfig = "config.json"
      nodeTopology = "topology.json"
      nodeByronGenesis = "byron-genesis.json"
      nodeShelleyGenesis = "shelley-genesis.json"
      nodeAlonzoGenesis = "alonzo-genesis.json"
      nodeConwayGenesis = "conway-genesis.json"
      envUrl = "https://book.world.dev.cardano.org/environments" </> networkDir network
  configDir <- getXdgDirectory XdgConfig ("cardano" </> networkDir network)
  createDirectoryIfMissing True configDir
  mapM_
    (retrieveConfigFile envUrl configDir)
    [ nodeConfig
    , nodeTopology
    , nodeByronGenesis
    , nodeShelleyGenesis
    , nodeAlonzoGenesis
    , nodeConwayGenesis
    ]
  pure (configDir </> nodeConfig, configDir </> nodeTopology)
 where
  retrieveConfigFile envUrl configDir config = do
    request <- parseRequest $ "GET " <> envUrl </> config
    let configFile = configDir </> config
    httpLBS request >>= LBS.writeFile configFile . getResponseBody
    putStrLn $ "Retrieved " <> configFile

-- | Generate command-line arguments for launching @cardano-node@.
cardanoNodeProcess :: FilePath -> Network -> IO CreateProcess
cardanoNodeProcess exe network = do
  (nodeConfigFile, nodeTopologyFile) <- findConfigFiles network
  nodeDatabaseDir <- getXdgDirectory XdgData ("cardano" </> networkDir network)
  createDirectoryIfMissing True nodeDatabaseDir
  nodeSocket <- findSocketPath network
  pure $
    proc exe $
      "run"
        : [ "--config"
          , nodeConfigFile
          , "--topology"
          , nodeTopologyFile
          , "--database-path"
          , nodeDatabaseDir </> "db"
          , "--socket-path"
          , nodeSocket
          ]

networkDir :: Network -> FilePath
networkDir = \case
  Preview -> "preview"
  Preprod -> "preprod"
  Mainnet -> "mainnet"

networkMagicArgs :: Network -> [String]
networkMagicArgs = \case
  Preview -> ["--testnet-magic", "2"]
  Preprod -> ["--testnet-magic", "1"]
  Mainnet -> ["--mainnet"]

withLogFile :: String -> (Handle -> IO a) -> IO a
withLogFile namespace k = do
  logFile <- findLogFile namespace
  putStrLn ("Logfile written to: " <> logFile)
  withFile logFile AppendMode (\out -> hSetBuffering out NoBuffering >> k out)

findLogFile :: String -> IO FilePath
findLogFile namespace = do
  logDir <- getXdgDirectory XdgCache namespace
  createDirectoryIfMissing True logDir
  pure $ logDir </> "node.log"

checkProcessHasNotDied :: Network -> String -> ProcessHandle -> IO Void
checkProcessHasNotDied network name processHandle = do
  logFile <- findLogFile (name </> networkDir network)
  waitForProcess processHandle >>= \case
    ExitSuccess -> error "Process has died"
    ExitFailure exit -> error $ "Process " <> show name <> " exited with failure code: " <> show exit <> ", check logs in " <> logFile
