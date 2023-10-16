{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Games.Run.Cardano where

import Control.Exception (finally, onException)
import Control.Monad (unless, when)
import Control.Monad.Class.MonadAsync (race)
import Control.Monad.Class.MonadTimer (threadDelay)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text, unpack)
import Data.Void (Void)
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest)
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesFileExist, getXdgDirectory, removeFile, Permissions (..), getPermissions, setPermissions, setOwnerExecutable)
import System.Exit (ExitCode (..))
import System.FilePath ((</>), (<.>))
import System.IO (BufferMode (..), Handle, IOMode (..), hSetBuffering, withFile)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), proc, waitForProcess, withCreateProcess, terminateProcess)
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip

data CardanoNode = CardanoNode
  { nodeSocket :: FilePath
  }
  deriving (Show)

withCardanoNode :: (CardanoNode -> IO a) -> IO a
withCardanoNode k =
  withLogFile "cardano-node" $ \out -> do
    exe <- findCardanoExecutable
    socketPath <- findSocketPath
    process <- cardanoNodeProcess exe
    withCreateProcess process{std_out = UseHandle out, std_err = UseHandle out} $
      \_stdin _stdout _stderr processHandle ->
        ( race
            (checkProcessHasNotDied "cardano-node" processHandle)
            (waitForNode socketPath k)
            >>= \case
              Left{} -> error "should never been reached"
              Right a -> pure a
        )
          `finally` (cleanupSocketFile socketPath >> terminateProcess processHandle)
 where
  waitForNode socketPath cont = do
    let rn = CardanoNode{nodeSocket = socketPath}
    waitForSocket rn
    cont rn

  cleanupSocketFile socketPath = do
    exists <- doesFileExist socketPath
    when exists $ removeFile socketPath

findSocketPath :: IO FilePath
findSocketPath = do
  socketDir <- getXdgDirectory XdgCache "cardano-node"
  createDirectoryIfMissing True socketDir
  pure $ socketDir </> "node.socket"

findCardanoExecutable :: IO FilePath
findCardanoExecutable = do
  dataDir <- getXdgDirectory XdgData "cardano"
  createDirectoryIfMissing True dataDir
  let cardanoExecutable = dataDir </> "cardano-node"
  exists <- doesFileExist cardanoExecutable
  unless exists $ downloadCardanoExecutable dataDir
  permissions <- getPermissions cardanoExecutable
  unless (executable permissions) $ setPermissions cardanoExecutable (setOwnerExecutable True permissions)
  pure cardanoExecutable

findCardanoCliExecutable :: IO FilePath
findCardanoCliExecutable = do
  dataDir <- getXdgDirectory XdgData "cardano"
  let cardanoCliExecutable = dataDir </> "cardano-cli"
  permissions <- getPermissions cardanoCliExecutable
  unless (executable permissions) $ setPermissions cardanoCliExecutable (setOwnerExecutable True permissions)
  pure cardanoCliExecutable

downloadCardanoExecutable :: FilePath -> IO ()
downloadCardanoExecutable destDir = do
  let binariesUrl = "https://github.com/input-output-hk/cardano-node/releases/download/8.1.2/cardano-node-8.1.2-macos.tar.gz"
  request <- parseRequest $ "GET " <> binariesUrl
  putStr "Downloading cardano executables"
  httpLBS request >>= Tar.unpack destDir . Tar.read . GZip.decompress . getResponseBody
  putStrLn " done"


-- | Wait for the node socket file to become available.
waitForSocket :: CardanoNode -> IO ()
waitForSocket node@CardanoNode{nodeSocket} = do
  exists <- doesFileExist nodeSocket
  unless exists $ do
    putStrLn "Cardano node syncing"
    threadDelay 1_000_000
    waitForSocket node

findConfigFiles :: IO (FilePath, FilePath)
findConfigFiles = do
  let nodeConfig = "config.json"
      nodeTopology = "topology.json"
      nodeByronGenesis = "byron-genesis.json"
      nodeShelleyGenesis = "shelley-genesis.json"
      nodeAlonzoGenesis = "alonzo-genesis.json"
      nodeConwayGenesis = "conway-genesis.json"
      envUrl = "https://book.world.dev.cardano.org/environments/preview"
  configDir <- getXdgDirectory XdgConfig "cardano"
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
cardanoNodeProcess :: FilePath -> IO CreateProcess
cardanoNodeProcess exe = do
  (nodeConfigFile, nodeTopologyFile) <- findConfigFiles
  nodeDatabaseDir <- getXdgDirectory XdgData "cardano"
  createDirectoryIfMissing True nodeDatabaseDir
  nodeSocket <- findSocketPath
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

findLogFile :: String -> IO FilePath
findLogFile namespace = do
  logDir <- getXdgDirectory XdgCache namespace
  createDirectoryIfMissing True logDir
  pure $ logDir </> namespace <.> "log"

withLogFile :: String -> (Handle -> IO a) -> IO a
withLogFile namespace k = do
  logFile <- findLogFile namespace
  withFile logFile AppendMode (\out -> hSetBuffering out NoBuffering >> k out)
    `onException` putStrLn ("Logfile written to: " <> logFile)

checkProcessHasNotDied :: Text -> ProcessHandle -> IO Void
checkProcessHasNotDied name processHandle = do
  logFile <- findLogFile (unpack name)
  waitForProcess processHandle >>= \case
    ExitSuccess -> error "Process has died"
    ExitFailure exit -> error $ "Process " <> show name <> " exited with failure code: " <> show exit <> ", check logs in " <> logFile
