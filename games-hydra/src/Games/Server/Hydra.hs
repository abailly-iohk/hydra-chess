{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
import Control.Monad (forever, when)
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
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (FromJSON (..), Pair)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Lazy as LBS
import Data.Either (rights)
import Data.Foldable (toList)
import Data.IORef (atomicWriteIORef, newIORef, readIORef)
import Data.Sequence (Seq ((:|>)), (|>))
import qualified Data.Sequence as Seq
import Data.String (IsString (..))
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import Game.Client.Console (Coins, SimpleUTxO (..), parseQueryUTxO)
import Game.Server (
  FromChain (..),
  Game (initialGame),
  HeadId,
  Host (..),
  Indexed (..),
  IsChain (..),
  Server (..),
  ServerException (..),
 )
import Game.Server.Mock (MockCoin (..))
import Games.Run.Cardano (Network, findCardanoCliExecutable, findSocketPath, networkMagicArgs)
import Games.Run.Hydra (KeyRole (Game), findEloScriptFile, findKeys, getScriptAddress, getUTxOFor)
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

withHydraServer :: forall g. (Game g) => Network -> HydraParty -> Host -> (Server g Hydra IO -> IO ()) -> IO ()
withHydraServer network me host k = do
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
              , closeHead = sendClose cnx events
              }
       in k server
 where
  pullEventsFromWs :: TVar IO (Seq (FromChain g Hydra)) -> Connection -> IO ()
  pullEventsFromWs events cnx =
    forever $
      WS.receiveData cnx >>= \dat ->
        case eitherDecode' dat of
          Left err ->
            atomically $ modifyTVar' events (|> OtherMessage (fromString $ show err))
          Right (Response{output}) -> case output of
            HeadIsInitializing headId parties ->
              atomically (modifyTVar' events (|> HeadCreated headId parties))
            HeadIsAborted headId _ ->
              atomically (modifyTVar' events (|> HeadClosed headId))
            HeadIsFinalized headId _ ->
              atomically (modifyTVar' events (|> HeadClosed headId))
            Committed headId party _utxo ->
              atomically (modifyTVar' events (|> FundCommitted headId party 0))
            HeadIsOpen headId utxo ->
              atomically (modifyTVar' events (|> HeadOpened headId))
            CommandFailed{} ->
              putStrLn "Command failed"
            PostTxOnChainFailed{postTxError} ->
              atomically $
                modifyTVar'
                  events
                  ( |>
                      OtherMessage
                        ( fromString $
                            Text.unpack $
                              decodeUtf8 $
                                LBS.toStrict $
                                  encode postTxError
                        )
                  )
            Greetings{} -> pure ()
            HeadIsClosed{} -> pure ()
            ReadyToFanout headId ->
              atomically (modifyTVar' events (|> HeadClosing headId))
            GetUTxOResponse{} -> pure ()
            RolledBack{} -> pure ()

  sendInit :: Connection -> TVar IO (Seq (FromChain g Hydra)) -> [Text] -> IO HeadId
  sendInit cnx events _unusedParties = do
    WS.sendTextData cnx (encode Init)
    timeout
      600_000_000
      ( waitFor events $ \case
          HeadCreated headId _ -> Just headId
          _ -> Nothing
      )
      >>= maybe (throwIO $ ServerException "Timeout (10m) waiting for head Id") pure

  sendCommit :: Connection -> TVar IO (Seq (FromChain g Hydra)) -> Host -> Integer -> HeadId -> IO ()
  sendCommit cnx events Host{host, port} amount _headId = go
   where
    go = do
      cardanoCliExe <- findCardanoCliExecutable
      (skFile, vkFile) <- findKeys Game network
      socketPath <- findSocketPath network

      -- find game token UTxO
      eloScriptFile <- snd <$> findEloScriptFile vkFile network
      scriptAddress <- getScriptAddress eloScriptFile network
      gameUTxO <- getUTxOFor network scriptAddress
      let gameToken = rights . fmap (parseQueryUTxO . pack) $ gameUTxO
      when (null gameToken) $ error $ "Failed to retrieve game token to commit from:\n" <> unlines gameUTxO

      -- commit is now external, so we need to handle query to the server, signature and then
      -- submission via the cardano-cli
      request <- parseRequest ("POST http://" <> unpack host <> ":" <> show port <> "/commit")
      response <- httpLBS $ setRequestBodyJSON (mkFullUTxO (Text.pack scriptAddress) Nothing (head gameToken)) request

      txFileRaw <-
        mkstemp "tx.raw" >>= \(fp, hdl) -> do
          LBS.hPutStr hdl $ responseBody response
          hClose hdl
          pure fp

      putStrLn $ "Wrote raw commit tx file " <> txFileRaw

      callProcess
        cardanoCliExe
        $ [ "transaction"
          , "sign"
          , "--tx-file"
          , txFileRaw
          , "--signing-key-file"
          , skFile
          , "--out-file"
          , txFileRaw <.> "signed"
          ]
          <> networkMagicArgs network

      putStrLn $ "Signed commit tx file " <> (txFileRaw <.> "signed")

      callProcess
        cardanoCliExe
        $ [ "transaction"
          , "submit"
          , "--tx-file"
          , txFileRaw <.> "signed"
          , "--socket-path"
          , socketPath
          ]
          <> networkMagicArgs network

      putStrLn $ "Submitted commit tx file " <> txFileRaw <.> "signed"

      timeout
        60_000_000
        ( waitFor events $ \case
            FundCommitted _ party _ | party == me -> Just ()
            _ -> Nothing
        )
        >>= maybe (putStrLn "Timeout (60s) waiting for commit to appear, please try again") pure

  playGame :: Connection -> HeadId -> Value -> IO ()
  playGame cnx _headId play =
    -- need to put the play as a redeemer for a transaction that consumes the current
    -- game state datum attached to game script, which implies this UTxO should exist
    -- somewhere, which means we need to create it at the opening of the head and /then/
    -- notify the game has started. Also, the server should maintain some state to retrieve
    -- the UTxO attached to the game
    putStrLn $ "playing " <> (Text.unpack $ decodeUtf8 $ LBS.toStrict $ encode play)

  sendClose :: Connection -> TVar IO (Seq (FromChain g Hydra)) -> HeadId -> IO ()
  sendClose cnx events _unusedHeadId = do
    WS.sendTextData cnx (encode Close)
    timeout
      600_000_000
      ( waitFor events $ \case
          HeadClosing headId -> Just ()
          _ -> Nothing
      )
      >>= maybe (throwIO $ ServerException "Timeout (10m) waiting for head Id") (const $ WS.sendTextData cnx (encode Fanout))

  restartGame :: Connection -> HeadId -> IO ()
  restartGame = error "not implemented"

  pollEvents :: TVar IO (Seq (FromChain g Hydra)) -> Integer -> Integer -> IO (Indexed g Hydra)
  pollEvents events (fromInteger -> start) (fromInteger -> count) = do
    history <- readTVarIO events
    let indexed =
          Indexed
            { lastIndex = fromIntegral $ length history
            , events = toList $ Seq.take count $ Seq.drop start history
            }
    pure indexed

--         {
--             "09d34606abdcd0b10ebc89307cbfa0b469f9144194137b45b7a04b273961add8#687": {
--                 "address": "addr1w9htvds89a78ex2uls5y969ttry9s3k9etww0staxzndwlgmzuul5",
--                 "value": {
--                     "lovelace": 7620669
--                 },
--                 "witness": {
--                   "datum": "02",
--                   "plutusV2Script": {
--                       "cborHex": "420606",
--                       "description": "",
--                       "type": "PlutusScriptV2"
--                   },
--                   "redeemer": "21"
--                 }
--             }
--         }
data FullUTxO = FullUTxO
  { txIn :: Text
  , address :: Text
  , value :: Coins
  , scriptInfo :: Maybe ScriptInfo
  }
  deriving stock (Eq, Show)

data ScriptInfo = ScriptInfo
  { datumHash :: Text
  , -- Hex-encoded CBOR of Datum
    datumWitness :: Text
  , -- Text enveloppe of script
    scriptWitness :: Value
  , -- Hex-encoded CBOR of redeemer
    redeemerWitness :: Text
  }
  deriving stock (Eq, Show)

instance ToJSON FullUTxO where
  toJSON FullUTxO{txIn, address, value, scriptInfo} =
    object
      [ fromText txIn
          .= object
            ( [ "address" .= address
              , "value" .= value
              ]
                <> maybe [] asJson scriptInfo
            )
      ]

asJson :: ScriptInfo -> [Pair]
asJson ScriptInfo{datumHash, datumWitness, scriptWitness, redeemerWitness} =
  [ "datumHash" .= datumHash
  , "witness"
      .= object
        [ "datum" .= datumWitness
        , "redeemer" .= redeemerWitness
        , "plutusV2Script" .= scriptWitness
        ]
  ]

mkFullUTxO :: Text -> Maybe ScriptInfo -> SimpleUTxO -> FullUTxO
mkFullUTxO address scriptInfo = \case
  SimpleUTxO{txIn, coins} ->
    FullUTxO{txIn, address, value = coins, scriptInfo}
  UTxOWithDatum{txIn, coins, datumHash} ->
    FullUTxO
      { txIn
      , address
      , value = coins
      , scriptInfo =
          fmap
            ( \ScriptInfo{datumWitness, redeemerWitness, scriptWitness} ->
                ScriptInfo{datumHash, ..}
            )
            scriptInfo
      }

waitFor :: TVar IO (Seq (FromChain g Hydra)) -> (FromChain g Hydra -> Maybe a) -> IO a
waitFor events predicate =
  atomically $ do
    readTVar events >>= \case
      (_ :|> event) -> maybe retry pure (predicate event)
      _ -> retry

data Request = Init | Close | Fanout
  deriving stock (Eq, Show, Generic)

instance ToJSON Request where
  toJSON = \case
    Init -> object ["tag" .= ("Init" :: Text)]
    Close -> object ["tag" .= ("Close" :: Text)]
    Fanout -> object ["tag" .= ("Fanout" :: Text)]

instance FromJSON Request where
  parseJSON = withObject "Request" $ \obj ->
    obj .: "tag" >>= \case
      ("Init" :: String) -> pure Init
      ("Close" :: String) -> pure Close
      ("Fanout" :: String) -> pure Close
      other -> fail $ "Unknown request type: " <> other

data Response = Response
  { output :: !Output
  , seq :: !Natural
  , timestamp :: !UTCTime
  }
  deriving stock (Eq, Show, Generic)

data Output
  = HeadIsInitializing {headId :: HeadId, parties :: [HydraParty]}
  | Committed {headId :: HeadId, party :: HydraParty, utxo :: Value}
  | HeadIsOpen {headId :: HeadId, utxo :: Value}
  | Greetings {me :: HydraParty}
  | HeadIsAborted {headId :: HeadId, utxo :: Value}
  | HeadIsFinalized {headId :: HeadId, utxo :: Value}
  | HeadIsClosed {headId :: HeadId, snapshotNumber :: Int, contestationDeadline :: UTCTime}
  | ReadyToFanout {headId :: HeadId}
  | PostTxOnChainFailed {postChainTx :: Value, postTxError :: Value}
  | RolledBack
  | CommandFailed {clientInput :: Request}
  | GetUTxOResponse {headId :: HeadId, utxo :: Value}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
--   | HeadIsContested {headId :: HeadId, snapshotNumber :: SnapshotNumber}
--   | TxSeen {headId :: HeadId, transaction :: tx}
--   | TxValid {headId :: HeadId, transaction :: tx}
--   | TxInvalid {headId :: HeadId, utxo :: UTxOType tx, transaction :: tx, validationError :: ValidationError}
--   | TxExpired {headId :: HeadId, transaction :: tx}
--   | SnapshotConfirmed
--       { headId :: HeadId
--       , snapshot :: Snapshot tx
--       , signatures :: MultiSignature (Snapshot tx)
--       }
--   | InvalidInput {reason :: String, input :: Text}
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
