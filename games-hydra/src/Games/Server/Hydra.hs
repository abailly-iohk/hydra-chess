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

import qualified Chess.Game as Chess
import qualified Chess.Plutus as Plutus
import Control.Concurrent.Class.MonadSTM (
  TVar,
  atomically,
  modifyTVar',
  newTVarIO,
  readTVar,
  readTVarIO,
  retry,
 )
import Control.Exception (Exception, IOException)
import Control.Monad (forever, when)
import Control.Monad.Class.MonadAsync (withAsync)
import Control.Monad.Class.MonadThrow (MonadCatch (catch), throwIO, try)
import Control.Monad.Class.MonadTime (UTCTime)
import Control.Monad.Class.MonadTimer (threadDelay, timeout)
import Data.Aeson (
  FromJSON,
  ToJSON (..),
  Value (..),
  decodeFileStrict',
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
import Game.Server (Content (..), FromChain (..), HeadId, Host (..), Indexed (..), IsChain (..), Server (..), ServerException (..))
import Game.Server.Mock (MockCoin (..))
import Games.Run.Cardano (Network, findCardanoCliExecutable, findSocketPath, networkMagicArgs)
import Games.Run.Hydra (KeyRole (Game), findDatumFile, findEloScriptFile, findGameScriptFile, findKeys, getScriptAddress, getUTxOFor, findPubKeyHash)
import Network.HTTP.Client (responseBody, responseStatus)
import Network.HTTP.Simple (httpLBS, parseRequest, setRequestBodyJSON)
import Network.HTTP.Types (statusCode)
import Network.WebSockets (Connection, runClient)
import qualified Network.WebSockets as WS
import Numeric.Natural (Natural)
import System.FilePath ((<.>))
import System.IO (hClose)
import System.Posix (mkstemp)
import System.Process (callProcess)
import Prelude hiding (seq)
import qualified Chess.Token as Token
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.Key as Key

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

data CommitError = CommitError String
  deriving (Eq, Show, Exception)

withHydraServer :: Network -> HydraParty -> Host -> (Server Chess.Game Hydra IO -> IO ()) -> IO ()
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
              , newGame = newGame events cnx
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
            HeadIsOpen headId utxo -> do
              splitGameUTxO utxo
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
            GetUTxOResponse{utxo} ->
              atomically (modifyTVar' events (|> OtherMessage (Content $ decodeUtf8 $ LBS.toStrict $ encode utxo)))
            RolledBack{} -> pure ()

  splitGameUTxO :: Value -> IO ()
  splitGameUTxO utxo = do
    myGameToken <- findGameToken utxo
    undefined

  findGameToken :: Value -> IO [String]
  findGameToken utxo = do
    pkh <- findKeys Game network >>= findPubKeyHash . snd
    let pid = Token.validatorHashHex
    pure $ extractGameToken pid pkh utxo


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

  -- (pkh, eloScriptFile) <- findEloScriptFile gameVk network
  -- let eloDatumValue :: Integer = 1000
  --     eloDatumHash =
  --       Text.unpack $
  --         decodeUtf8 $
  --           Hex.encode $
  --             datumHashBytes eloDatumValue
  -- eloScriptAddress <- getScriptAddress eloScriptFile network


  sendCommit :: Connection -> TVar IO (Seq (FromChain g Hydra)) -> Host -> Integer -> HeadId -> IO ()
  sendCommit cnx events Host{host, port} amount _headId =
    try go >>= \case
      Left (CommitError msg) -> putStrLn msg
      Right{} -> pure ()
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

      scriptWitness <-
        decodeFileStrict' eloScriptFile >>= \case
          Nothing -> throwIO $ CommitError $ "Cannot decode script file " <> eloScriptFile
          Just v -> pure v

      -- build script info
      let scriptInfo =
            ScriptInfo
              { datumWitness = Plutus.datumBytes (1000 :: Integer) -- TODO: get real value from somehwere
              , redeemerWitness = Plutus.datumBytes ()
              , scriptWitness
              }
          utxo = mkFullUTxO (Text.pack scriptAddress) (Just scriptInfo) (head gameToken)

      -- commit is now external, so we need to handle query to the server, signature and then
      -- submission via the cardano-cli
      request <- parseRequest ("POST http://" <> unpack host <> ":" <> show port <> "/commit")
      response <- httpLBS $ setRequestBodyJSON (utxo) request

      txFileRaw <-
        case statusCode (responseStatus response) of
          200 ->
            mkstemp "tx.raw" >>= \(fp, hdl) -> do
              LBS.hPutStr hdl $ responseBody response
              hClose hdl
              pure fp
          other ->
            throwIO $
              CommitError $
                "Commit transaction failed with error "
                  <> show other
                  <> " for UTxO "
                  <> (unpack $ decodeUtf8 $ LBS.toStrict (encode utxo))

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

  newGame :: TVar IO (Seq (FromChain g Hydra)) -> Connection -> HeadId -> IO ()
  newGame events connection headId = do
    cardanoCliExe <- findCardanoCliExecutable
    (skFile, vkFile) <- findKeys Game network
    socketPath <- findSocketPath network

    -- find ELO script for witnessing
    eloScriptFile <- snd <$> findEloScriptFile vkFile network
    let datumWitness = Plutus.datumBytes (1000 :: Integer) -- TODO: get real value from somehwere
        redeemerWitness = Plutus.datumBytes ()

    -- find chess game address
    gameScriptFile <- findGameScriptFile network
    gameScriptAddress <- getScriptAddress gameScriptFile network

    -- construct chess game inline datum
    gameDatumFile <- findDatumFile "game-state" Chess.initialGame network

    utxo <- collectUTxO events connection
    -- let inputs = buildInput <$> utxo

    -- % cardano-cli transaction build-raw --tx-in c5a00b09e82c334bd04d62313ab25608ca70e7d1014ca9c8dfc09251d51ea6a0#0 --tx-in-script-file ~/.config/hydra-node/preview/elo-script.plutus --tx-in-datum-value 1000 --tx-in-redeemer-value '[]' --tx-in-execution-units '(0,0)' --tx-in-collateral c5a00b09e82c334bd04d62313ab25608ca70e7d1014ca9c8dfc09251d51ea6a0#0 --tx-out 'addr_test1wrr66kuw94l4zh7jff227872cyppk2ttketmdsqcflwhwhchp6lwz+10000000 + 1 e18ad836532a69a93160efe11bcfac05b812a092ef3420042e700c10.1ad7cb51c9e2d6250bd80395a5c920f6f628adc4b1bd057a81c9be98' --tx-out-inline-datum-file ~/.config/hydra-node/preview/chess-game-state.json --fee 0 --protocol-params-file ~/.config/hydra-node/preview/protocol-parameters.json --out-file game-tx.raw

    putStrLn $
      unlines
        [ "newGame:"
        , "datumWitness: " <> (unpack $ decodeUtf8 $ Hex.encode datumWitness)
        , "redeemerWitness: " <> (unpack $ decodeUtf8 $ Hex.encode redeemerWitness)
        , "gameScriptAddress: " <> gameScriptAddress
        , "gameDatum: " <> gameDatumFile
        , "utxo: " <> (unpack utxo)
        ]
  --     args =
  --       [ "transaction"
  --       , "build"
  --       , "--tx-in"
  --       , txin
  --       , "--tx-in-collateral"
  --       , txin
  --       , "--tx-out"
  --       , eloScriptAddress <> " + 10000000 lovelace + " <> token
  --       , "--tx-out-datum-hash"
  --       , eloDatumHash
  --       , "--out-file"
  --       , txFileRaw <.> "raw"
  --       , "--socket-path"
  --       , socketPath
  --       ]
  --         <> networkMagicArgs network

  -- putStrLn $ "Building new game transaction " <> (txFileRaw <.> "raw") <> " with arguments: " <> unwords args

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

  pollEvents :: TVar IO (Seq (FromChain g Hydra)) -> Integer -> Integer -> IO (Indexed g Hydra)
  pollEvents events (fromInteger -> start) (fromInteger -> count) = do
    history <- readTVarIO events
    let indexed =
          Indexed
            { lastIndex = fromIntegral $ length history
            , events = toList $ Seq.take count $ Seq.drop start history
            }
    pure indexed

  -- Collect only TxIn
  collectUTxO :: TVar IO (Seq (FromChain g Hydra)) -> Connection -> IO Text
  collectUTxO events cnx = do
    WS.sendTextData cnx (encode GetUTxO)
    timeout
      600_000_000
      ( waitFor events $ \case
          OtherMessage (Content txt) -> Just txt
          _ -> Nothing
      )
      >>= maybe (throwIO $ ServerException "Timeout (10m) waiting for GetUTxO") pure

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
  , datumhash :: Maybe Text
  , scriptInfo :: Maybe ScriptInfo
  }
  deriving stock (Eq, Show)

data ScriptInfo = ScriptInfo
  { datumWitness :: ByteString
  -- ^ CBOR of datum
  , scriptWitness :: Value
  -- ^ Text enveloppe of script
  , redeemerWitness :: ByteString
  -- ^ CBOR of redeemer
  }
  deriving stock (Eq, Show)

instance ToJSON FullUTxO where
  toJSON FullUTxO{txIn, address, value, datumhash, scriptInfo} =
    object
      [ fromText txIn
          .= object
            ( [ "address" .= address
              , "value" .= value
              , "datumhash" .= datumhash
              ]
                <> maybe [] asJson scriptInfo
            )
      ]

asJson :: ScriptInfo -> [Pair]
asJson ScriptInfo{datumWitness, scriptWitness, redeemerWitness} =
  [ "witness"
      .= object
        [ "datum" .= (decodeUtf8 $ Hex.encode $ datumWitness)
        , "redeemer" .= (decodeUtf8 $ Hex.encode $ redeemerWitness)
        , "plutusV2Script" .= scriptWitness
        ]
  ]

mkFullUTxO :: Text -> Maybe ScriptInfo -> SimpleUTxO -> FullUTxO
mkFullUTxO address scriptInfo = \case
  SimpleUTxO{txIn, coins} ->
    FullUTxO{txIn, address, value = coins, scriptInfo, datumhash = Nothing}
  UTxOWithDatum{txIn, coins, datumhash} ->
    FullUTxO
      { txIn
      , address
      , value = coins
      , scriptInfo
      , datumhash = Just datumhash
      }

waitFor :: TVar IO (Seq (FromChain g Hydra)) -> (FromChain g Hydra -> Maybe a) -> IO a
waitFor events predicate =
  atomically $ do
    readTVar events >>= \case
      (_ :|> event) -> maybe retry pure (predicate event)
      _ -> retry

data Request = Init | Close | Fanout | GetUTxO
  deriving stock (Eq, Show, Generic)

instance ToJSON Request where
  toJSON = \case
    Init -> object ["tag" .= ("Init" :: Text)]
    Close -> object ["tag" .= ("Close" :: Text)]
    Fanout -> object ["tag" .= ("Fanout" :: Text)]
    GetUTxO -> object ["tag" .= ("GetUTxO" :: Text)]

instance FromJSON Request where
  parseJSON = withObject "Request" $ \obj ->
    obj .: "tag" >>= \case
      ("Init" :: String) -> pure Init
      ("Close" :: String) -> pure Close
      ("Fanout" :: String) -> pure Close
      ("GetUTxO" :: String) -> pure GetUTxO
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

extractGameToken :: String -> String -> Value -> [String]
extractGameToken pid pkh = \case
    Object kv -> foldMap (findUTxOWithValue pid pkh) (KeyMap.toList kv)
    _ -> []

findUTxOWithValue :: String -> String -> (KeyMap.Key, Value) -> [String]
findUTxOWithValue pid pkh (txin, txout) =
  case txout of
    Object kv -> if hasValueFor pid pkh kv
                 then [unpack $ Key.toText txin]
                 else []
    _ -> []

hasValueFor :: String -> String -> Aeson.Object -> Bool
hasValueFor pid pkh kv =
  case filter ((== fromString "value") . fst) $ KeyMap.toList kv of
     [ (_, Object val) ] ->
       case filter ((== fromString pid) . fst) $ KeyMap.toList val  of
         [ (_, Object tok)] ->
           case filter ((== fromString pkh) . fst) $ KeyMap.toList tok  of
             [] -> False
             _ -> True
         _ -> False
     _ -> False
