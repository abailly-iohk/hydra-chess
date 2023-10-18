{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Games.Server.Hydra where

import Control.Concurrent.Class.MonadSTM (
  TVar,
  atomically,
  modifyTVar',
  newTVarIO,
  readTVar,
  readTVarIO,
  retry,
 )
import Control.Exception (IOException)
import Control.Monad (forever)
import Control.Monad.Class.MonadAsync (withAsync)
import Control.Monad.Class.MonadThrow (MonadCatch (catch), throwIO)
import Control.Monad.Class.MonadTime (UTCTime)
import Control.Monad.Class.MonadTimer (threadDelay, timeout)
import Data.Aeson (
  FromJSON,
  ToJSON (..),
  Value,
  eitherDecode',
  encode,
  object,
  withObject,
  (.:),
  (.=),
 )
import Data.Aeson.Types (FromJSON (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import Data.IORef (atomicWriteIORef, newIORef, readIORef)
import Data.Sequence (Seq ((:|>)), (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import Game.Server (
  FromChain (..),
  HeadId,
  Host (..),
  Indexed (..),
  IsChain (..),
  Server (..),
  ServerException (..),
 )
import Game.Server.Mock (MockCoin (..))
import Games.Run.Cardano (findCardanoCliExecutable, findSocketPath)
import Games.Run.Hydra (findCardanoSigningKey)
import Network.HTTP.Client (responseBody)
import Network.HTTP.Simple (httpLBS, parseRequest, setRequestBodyJSON)
import Network.WebSockets (Connection, runClient)
import qualified Network.WebSockets as WS
import Numeric.Natural (Natural)
import System.FilePath ((<.>))
import System.IO (hClose)
import System.Posix (mkstemp)
import System.Process (callProcess)
import Prelude hiding (seq)

-- | The type of backend provide by Hydra
data Hydra

-- | A hydra party is identified by its hydra verification key
data HydraParty = HydraParty {vkey :: ByteString}
  deriving (Eq)

instance Show HydraParty where
  show HydraParty{vkey} = unpack . decodeUtf8 . Hex.encode $ vkey

instance ToJSON HydraParty where
  toJSON HydraParty{vkey} =
    object ["vkey" .= (unpack . decodeUtf8 . Hex.encode $ vkey)]

instance FromJSON HydraParty where
  parseJSON = withObject "HydraParty" $ \obj ->
    (obj .: "vkey")
      >>= ( \case
              Left err -> fail err
              Right v -> pure $ HydraParty v
          )
        . Hex.decode
        . encodeUtf8

instance IsChain Hydra where
  type Party Hydra = HydraParty
  type Coin Hydra = MockCoin

  partyId = pack . show . vkey

  coinValue (MockCoin c) = c

withHydraServer :: HydraParty -> Host -> (Server g Hydra IO -> IO ()) -> IO ()
withHydraServer me host k = do
  events <- newTVarIO mempty
  withClient host $ \cnx ->
    withAsync (pullEventsFromWs events cnx) $ \_ ->
      let server =
            Server
              { initHead = sendInit cnx events
              , poll = pollEvents events
              , commit = sendCommit cnx events host
              , play = playGame cnx
              , newGame = restartGame cnx
              , closeHead = sendClose cnx
              }
       in k server
 where
  pullEventsFromWs :: TVar IO (Seq (FromChain g Hydra)) -> Connection -> IO ()
  pullEventsFromWs events cnx =
    forever $
      WS.receiveData cnx >>= \dat ->
        case eitherDecode' dat of
          Left err -> putStrLn (show err <> ", data: " <> show dat)
          Right (Response{output = HeadIsInitializing headId parties}) ->
            atomically (modifyTVar' events (|> HeadCreated headId parties))
          Right (Response{output = Committed headId party _utxo}) ->
            atomically (modifyTVar' events (|> FundCommitted headId party 0))

  sendInit :: Connection -> TVar IO (Seq (FromChain g Hydra)) -> [Text] -> IO HeadId
  sendInit cnx events _unusedParties = do
    WS.sendTextData cnx (encode Init)
    timeout
      60_000_000
      ( waitFor events $ \case
          HeadCreated headId _ -> Just headId
          _ -> Nothing
      )
      >>= maybe (throwIO $ ServerException "Timeout (60s) waiting for head Id") pure

  sendCommit :: Connection -> TVar IO (Seq (FromChain g Hydra)) -> Host -> Integer -> HeadId -> IO ()
  sendCommit cnx events Host{host, port} amount _headId = go
   where
    go = do
      -- commit is now external, so we need to handle query to the server, signature and then
      -- submission via the cardano-cli
      request <- parseRequest ("POST http://" <> unpack host <> ":" <> show port <> "/commit")
      -- where does the committed UTxO comes from?
      -- can go for empty commit for now
      response <- httpLBS $ setRequestBodyJSON (object []) request

      txFileRaw <-
        mkstemp "tx.raw" >>= \(fp, hdl) -> do
          LBS.hPutStr hdl $ responseBody response
          hClose hdl
          pure fp

      putStrLn $ "Wrote raw commit tx file " <> txFileRaw

      cardanoCliExe <- findCardanoCliExecutable
      skFile <- findCardanoSigningKey
      socketPath <- findSocketPath

      callProcess
        cardanoCliExe
        [ "transaction"
        , "sign"
        , "--tx-file"
        , txFileRaw
        , "--signing-key-file"
        , skFile
        , "--testnet-magic"
        , "2"
        , "--out-file"
        , txFileRaw <.> "signed"
        ]

      putStrLn $ "Signed commit tx file " <> (txFileRaw <.> "signed")

      callProcess
        cardanoCliExe
        [ "transaction"
        , "submit"
        , "--tx-file"
        , txFileRaw <.> "signed"
        , "--testnet-magic"
        , "2"
        , "--socket-path"
        , socketPath
        ]

      putStrLn $ "Submitted commit tx file " <> txFileRaw <.> "signed"

      timeout
        60_000_000
        ( waitFor events $ \case
            -- where does our party identifier comes from
            FundCommitted _ party _ | party == me -> Just ()
            _ -> Nothing
        )
        >>= maybe (putStrLn "Timeout (60s) waiting for commit to appear, please try again") pure

  sendClose :: Connection -> HeadId -> IO ()
  sendClose = error "not implemented"

  restartGame :: Connection -> HeadId -> IO ()
  restartGame = error "not implemented"

  playGame :: Connection -> HeadId -> Value -> IO ()
  playGame = error "not implemented"

  pollEvents :: TVar IO (Seq (FromChain g Hydra)) -> Integer -> Integer -> IO (Indexed g Hydra)
  pollEvents events (fromInteger -> start) (fromInteger -> count) = do
    history <- readTVarIO events
    let indexed =
          Indexed
            { lastIndex = fromIntegral $ length history
            , events = toList $ Seq.take count $ Seq.drop start history
            }
    pure indexed

waitFor :: TVar IO (Seq (FromChain g Hydra)) -> (FromChain g Hydra -> Maybe a) -> IO a
waitFor events predicate =
  atomically $ do
    readTVar events >>= \case
      (_ :|> event) -> maybe retry pure (predicate event)
      _ -> retry

data Request = Init
  deriving stock (Eq, Show, Generic)

instance ToJSON Request where
  toJSON Init = object ["tag" .= ("Init" :: Text)]

data Response = Response
  { output :: !Output
  , seq :: !Natural
  , timestamp :: !UTCTime
  }
  deriving stock (Eq, Show, Generic)

data Output
  = HeadIsInitializing {headId :: HeadId, parties :: [HydraParty]}
  | Committed {headId :: HeadId, party :: HydraParty, utxo :: Value}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

instance FromJSON Response where
  parseJSON v = flip (withObject "Response") v $ \o -> do
    output <- parseJSON v
    seq <- o .: "seq"
    timestamp <- o .: "timestamp"
    pure $ Response{output, seq, timestamp}

-- -- | Individual server output messages as produced by the 'Hydra.HeadLogic' in
-- -- the 'ClientEffect'.
-- data ServerOutput tx
--   = PeerConnected {peer :: NodeId}
--   | PeerDisconnected {peer :: NodeId}
--   | HeadIsInitializing {headId :: HeadId, parties :: Set Party}
--   | Committed {headId :: HeadId, party :: Party, utxo :: UTxOType tx}
--   | HeadIsOpen {headId :: HeadId, utxo :: UTxOType tx}
--   | HeadIsClosed
--       { headId :: HeadId
--       , snapshotNumber :: SnapshotNumber
--       , -- | Nominal deadline until which contest can be submitted and after
--         -- which fanout is possible. NOTE: Use this only for informational
--         -- purpose and wait for 'ReadyToFanout' instead before sending 'Fanout'
--         -- as the ledger of our cardano-node might not have progressed
--         -- sufficiently in time yet and we do not re-submit transactions (yet).
--         contestationDeadline :: UTCTime
--       }
--   | HeadIsContested {headId :: HeadId, snapshotNumber :: SnapshotNumber}
--   | ReadyToFanout {headId :: HeadId}
--   | HeadIsAborted {headId :: HeadId, utxo :: UTxOType tx}
--   | HeadIsFinalized {headId :: HeadId, utxo :: UTxOType tx}
--   | CommandFailed {clientInput :: ClientInput tx}
--   | TxSeen {headId :: HeadId, transaction :: tx}
--   | TxValid {headId :: HeadId, transaction :: tx}
--   | TxInvalid {headId :: HeadId, utxo :: UTxOType tx, transaction :: tx, validationError :: ValidationError}
--   | TxExpired {headId :: HeadId, transaction :: tx}
--   | SnapshotConfirmed
--       { headId :: HeadId
--       , snapshot :: Snapshot tx
--       , signatures :: MultiSignature (Snapshot tx)
--       }
--   | GetUTxOResponse {headId :: HeadId, utxo :: UTxOType tx}
--   | InvalidInput {reason :: String, input :: Text}
--   | -- | A friendly welcome message which tells a client something about the
--     -- node. Currently used for knowing what signing key the server uses (it
--     -- only knows one).
--     Greetings {me :: Party}
--   | PostTxOnChainFailed {postChainTx :: PostChainTx tx, postTxError :: PostTxError tx}
--   | RolledBack
--   deriving (Generic)

withClient :: Host -> (Connection -> IO a) -> IO a
withClient Host{host, port} action = do
  connectedOnce <- newIORef False
  tryConnect connectedOnce
 where
  tryConnect connectedOnce =
    doConnect connectedOnce `catch` \(e :: IOException) -> do
      readIORef connectedOnce >>= \case
        False -> do
          putStrLn ("Failed to connect, retrying " <> unpack host <> ":" <> show port)
          threadDelay 1_000_000
          tryConnect connectedOnce
        True -> throwIO e

  doConnect connectedOnce = runClient (unpack host) port "/" $ \connection -> do
    atomicWriteIORef connectedOnce True
    putStrLn $ "Connected to " <> unpack host <> ":" <> show port
    res <- action connection
    WS.sendClose connection ("Bye" :: Text)
    pure res
