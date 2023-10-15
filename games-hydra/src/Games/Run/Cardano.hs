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
import Data.Text (Text)
import Data.Void (Void)
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest)
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesFileExist, getXdgDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (BufferMode (..), Handle, IOMode (..), hSetBuffering, withFile)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), proc, waitForProcess, withCreateProcess)

data CardanoNode = CardanoNode
  { nodeSocket :: FilePath
  }
  deriving (Show)

withCardanoNode :: (CardanoNode -> IO a) -> IO a
withCardanoNode k =
  withLogFile $ \out -> do
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
          `finally` cleanupSocketFile socketPath
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
  socketDir <- getXdgDirectory XdgCache "cardano"
  createDirectoryIfMissing True socketDir
  pure $ socketDir </> "node.socket"

findCardanoExecutable :: IO FilePath
findCardanoExecutable = do
  dataDir <- getXdgDirectory XdgData "cardano"
  createDirectoryIfMissing True dataDir
  pure $ dataDir </> "cardano-node"

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
    putStrLn $ "Retrieving " <> configFile

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

findLogFile :: IO FilePath
findLogFile = do
  logDir <- getXdgDirectory XdgCache "cardano"
  createDirectoryIfMissing True logDir
  pure $ logDir </> "cardano-node.log"

withLogFile :: (Handle -> IO a) -> IO a
withLogFile k = do
  logFile <- findLogFile
  withFile logFile AppendMode (\out -> hSetBuffering out NoBuffering >> k out)
    `onException` putStrLn ("Logfile written to: " <> logFile)

checkProcessHasNotDied :: Text -> ProcessHandle -> IO Void
checkProcessHasNotDied name processHandle = do
  logFile <- findLogFile
  waitForProcess processHandle >>= \case
    ExitSuccess -> error "Process has died"
    ExitFailure exit -> error $ "Process " <> show name <> " exited with failure code: " <> show exit <> ", check logs in " <> logFile
