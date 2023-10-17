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

module Game.Server.Hydra where

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
  eitherDecode',
  encode,
  object,
  withObject,
  (.:),
  (.=),
 )
import Data.Aeson.Types (FromJSON (..))
import Data.Foldable (toList)
import Data.IORef (atomicWriteIORef, newIORef, readIORef)
import Data.Sequence (Seq ((:|>)), (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text, unpack)
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
import Network.WebSockets (Connection, runClient)
import qualified Network.WebSockets as WS
import Numeric.Natural (Natural)
import Prelude hiding (seq)

-- | The type of backend provide by Hydra
data Hydra

instance IsChain Hydra where
  type Party Hydra = Text
  type Coin Hydra = MockCoin

  partyId = id

  coinValue (MockCoin c) = c

withHydraServer :: Host -> (Server g Hydra IO -> IO ()) -> IO ()
withHydraServer host k = do
  events <- newTVarIO mempty
  withClient host $ \cnx ->
    withAsync (pullEventsFromWs events cnx) $ \_ ->
      let server =
            Server
              { initHead = sendInit cnx events
              , poll = pollEvents events
              , commit = sendCommit cnx
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

  sendInit :: Connection -> TVar IO (Seq (FromChain g Hydra)) -> [Text] -> IO HeadId
  sendInit cnx events parties = do
    WS.sendTextData cnx (encode Init)
    timeout 60_000_000 (waitForHeadId events parties)
      >>= maybe (throwIO $ ServerException "Timeout (60s) waiting for head Id") pure

  sendClose :: Connection -> HeadId -> IO ()
  sendClose = error "not implemented"

  restartGame :: Connection -> HeadId -> IO ()
  restartGame = error "not implemented"

  playGame :: Connection -> HeadId -> Int -> IO ()
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

  sendCommit :: Connection -> Integer -> HeadId -> IO ()
  sendCommit = error "not implemented"

waitForHeadId :: TVar IO (Seq (FromChain g Hydra)) -> [Text] -> IO HeadId
waitForHeadId events parties =
  atomically $ do
    readTVar events >>= \case
      (_ :|> HeadCreated headId ps) | ps == parties -> pure headId
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

data Output = HeadIsInitializing {headId :: HeadId, parties :: [Text]}
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
