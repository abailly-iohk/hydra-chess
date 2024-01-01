{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Games.Server.Hydra where

import Chess.Game (Check (..), Side (..))
import qualified Chess.Game as Chess
import Chess.GameState (ChessGame (..), ChessPlay (..))
import Chess.Plutus (fromJSONDatum, pubKeyHashFromHex)
import qualified Chess.Token as Token
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
  eitherDecode',
  encode,
  object,
  withObject,
  (.:),
  (.=),
 )
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (FromJSON (..), Pair)
import qualified Data.Aeson.Types as Aeson
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Lazy as LBS
import Data.Either (rights)
import Data.Foldable (toList)
import Data.IORef (atomicWriteIORef, newIORef, readIORef)
import Data.List (intersperse)
import qualified Data.List as List
import Data.Scientific (floatingOrInteger)
import Data.Sequence (Seq ((:|>)), (|>))
import qualified Data.Sequence as Seq
import Data.String (IsString (..))
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import Game.Chess (Chess, ChessEnd (..), GamePlay (..))
import Game.Client.Console (Coins, SimpleUTxO (..), parseQueryUTxO)
import Game.Server (
  Content (..),
  FromChain (..),
  HeadId,
  Host (..),
  Indexed (..),
  IsChain (..),
  Server (..),
  ServerException (..),
 )
import Game.Server.Mock (MockCoin (..))
import Games.Run.Cardano (Network, findCardanoCliExecutable, findSocketPath, networkMagicArgs)
import Games.Run.Hydra (
  KeyRole (Game),
  extractCBORHex,
  findDatumFile,
  findEloScriptFile,
  findGameScriptFile,
  findKeys,
  findProtocolParametersFile,
  findPubKeyHash,
  getScriptAddress,
  getUTxOFor,
  getVerificationKeyAddress,
  makeEloScriptFile,
  mkTempFile,
 )
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

type GameTokens = [(String, String, Integer)]

data InvalidMove
  = InvalidMove Chess.IllegalMove
  | InvalidGameTokens GameTokens GameTokens
  | UTxOError Text
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
              , play = playGame cnx events
              , newGame = newGame events cnx
              , closeHead = sendClose cnx events
              }
       in k server
 where
  pullEventsFromWs :: TVar IO (Seq (FromChain Chess Hydra)) -> Connection -> IO ()
  pullEventsFromWs events cnx =
    forever $
      WS.receiveData cnx >>= \dat ->
        case eitherDecode' dat of
          Left err ->
            atomically $ modifyTVar' events (|> OtherMessage (Content $ pack err))
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
              splitGameUTxO cnx utxo
              atomically (modifyTVar' events (|> HeadOpened headId))
            CommandFailed{} ->
              putStrLn "Command failed"
            PostTxOnChainFailed{postTxError} ->
              atomically $
                modifyTVar'
                  events
                  ( |>
                      OtherMessage
                        ( Content $
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
              atomically (modifyTVar' events (|> OtherMessage (JsonContent utxo)))
            RolledBack{} -> pure ()
            TxValid{} -> pure ()
            TxInvalid{validationError} ->
              atomically $ modifyTVar' events (|> OtherMessage (Content $ pack $ asString validationError))
            SnapshotConfirmed{headId, snapshot = Snapshot{utxo}} ->
              handleGameState events cnx headId utxo

  handleGameState :: TVar IO (Seq (FromChain Chess Hydra)) -> Connection -> HeadId -> Value -> IO ()
  handleGameState events cnx headId utxo = do
    -- find output paying to game script address
    gameScriptFile <- findGameScriptFile network
    gameScriptAddress <- getScriptAddress gameScriptFile network
    -- extract game state from inline datum encoded as Data with schema
    case extractGameState gameScriptAddress utxo of
      Left err -> pure ()
      Right st@ChessGame{game} ->
        if
            | game == Chess.initialGame ->
                atomically $ modifyTVar' events (|> GameStarted headId st [])
            | Chess.checkState game == CheckMate White -> do
                atomically $ modifyTVar' events (|> GameEnded headId st BlackWins)
                endGame events cnx headId utxo
            | Chess.checkState game == CheckMate Black -> do
                atomically $ modifyTVar' events (|> GameEnded headId st WhiteWins)
                endGame events cnx headId utxo
            | otherwise ->
                atomically $ modifyTVar' events (|> GameChanged headId st [])

  splitGameUTxO :: Connection -> Value -> IO ()
  splitGameUTxO cnx utxo = do
    myGameToken <- findGameToken utxo
    case myGameToken of
      Nothing -> putStrLn ("Cannot find game token in " <> unpack (decodeUtf8 $ LBS.toStrict $ encode utxo)) >> pure ()
      Just txin -> postSplitTx cnx txin

  postSplitTx :: Connection -> String -> IO ()
  postSplitTx cnx txin = do
    (sk, vk) <- findKeys Game network
    gameAddress <- getVerificationKeyAddress vk network
    pkh <- findPubKeyHash vk
    let pid = Token.validatorHashHex
        token = "1 " <> pid <> "." <> pkh

    (_, eloScriptFile) <- findEloScriptFile vk network
    eloScriptAddress <- getScriptAddress eloScriptFile network

    let args =
          [ "--tx-in"
          , txin
          , "--tx-out"
          , gameAddress <> "+8000000" -- ADA part
          , "--tx-out"
          , eloScriptAddress <> "+ 2000000 lovelace + " <> token
          , "--tx-out-inline-datum-value"
          , "1000" -- FIXME: should be some ELO rating, but where does it come from?
          ]

    submitNewTx cnx args sk

  submitNewTx cnx args sk = do
    cardanoCliExe <- findCardanoCliExecutable
    socketPath <- findSocketPath network
    protocolParametersFile <- findProtocolParametersFile network
    txRawFile <- mkTempFile
    let fullArgs =
          [ "transaction"
          , "build-raw"
          ]
            <> args
            <> [ "--fee"
               , "0"
               , "--protocol-params-file"
               , protocolParametersFile
               , "--out-file"
               , txRawFile
               ]

    putStrLn $ "Creating raw tx with " <> unwords fullArgs

    callProcess cardanoCliExe fullArgs

    putStrLn $ "Created raw split tx file " <> txRawFile <> " with args: '" <> unwords args <> "'"

    callProcess
      cardanoCliExe
      $ [ "transaction"
        , "sign"
        , "--tx-file"
        , txRawFile
        , "--signing-key-file"
        , sk
        , "--out-file"
        , txRawFile <.> "signed"
        ]
        <> networkMagicArgs network

    putStrLn $ "Signed split tx file " <> (txRawFile <.> "signed")

    cborHex <- extractCBORHex $ txRawFile <.> "signed"

    putStrLn $ "Submitting tx " <> unpack cborHex

    WS.sendTextData cnx (encode $ NewTx cborHex)

    putStrLn $ "Submitted commit tx file " <> txRawFile <.> "signed"

  findGameToken :: Value -> IO (Maybe String)
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
      gameAddress <- getVerificationKeyAddress vkFile network
      gameUTxO <- getUTxOFor network gameAddress
      let gameToken = rights . fmap (parseQueryUTxO . pack) $ gameUTxO
      -- FIXME: Need to find the correct game token otherwise splitting won't work
      when (null gameToken) $ error $ "Failed to retrieve game token to commit from:\n" <> unlines gameUTxO

      let utxo = mkFullUTxO (Text.pack gameAddress) Nothing (head gameToken)

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
                  <> (asString utxo)

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

  -- \| the new game transaction
  --
  -- It is made of:
  -- \* each game token as input
  -- \* the current player's "deposit" as collateral
  -- \* a single game script output with the initial game
  -- \* the refunded deposit
  --
  -- The reason we need the deposit input is to provide an ADA-only collateral input
  -- which is required by the ledger rules
  newGame :: TVar IO (Seq (FromChain g Hydra)) -> Connection -> HeadId -> IO ()
  newGame events connection headId = do
    cardanoCliExe <- findCardanoCliExecutable
    (skFile, vkFile) <- findKeys Game network
    gameAddress <- getVerificationKeyAddress vkFile network
    socketPath <- findSocketPath network

    utxo <- collectUTxO events connection

    let collateral = findCollateral gameAddress utxo
        pid = Token.validatorHashHex
        gameTokens = findGameTokens pid utxo
        lovelace = sum $ (\(_, _, l) -> l) <$> gameTokens

    gameInputs <- concat <$> mapM makeGameInput gameTokens

    -- find chess game address
    gameScriptFile <- findGameScriptFile network
    gameScriptAddress <- getScriptAddress gameScriptFile network

    let initGame =
          ChessGame
            { players = fmap (\(_, pkh, _) -> pubKeyHashFromHex (pack pkh)) gameTokens
            , game = Chess.initialGame
            }

    -- construct chess game inline datum
    gameDatumFile <- findDatumFile "game-state" initGame network

    protocolParametersFile <- findProtocolParametersFile network
    txRawFile <- mkTempFile

    let args =
          gameInputs
            <> ["--tx-in", collateral]
            <> ["--tx-in-collateral", collateral]
            <> [ "--tx-out"
               , gameScriptAddress
                  <> "+ "
                  <> show lovelace
                  <> " lovelace + "
                  <> concat (intersperse " + " (makeValue pid <$> gameTokens))
               , "--tx-out-inline-datum-file"
               , gameDatumFile
               ]
            <> [ "--tx-out"
               , gameAddress <> "+ 8000000 lovelace" -- FIXME shoudl be value in collateral
               ]

    submitNewTx connection args skFile

  playGame :: Connection -> TVar IO (Seq (FromChain Chess Hydra)) -> HeadId -> GamePlay Chess -> IO ()
  playGame cnx events _headId (GamePlay End) = pure ()
  playGame cnx events _headId (GamePlay play@(ChessMove move)) =
    try go >>= \case
      Left (InvalidMove illegal) -> putStrLn $ "Invalid move:  " <> show illegal
      Left other -> putStrLn $ "Error looking for data:  " <> show other
      Right{} -> pure ()
   where
    go = do
      -- retrieve needed tools and keys
      cardanoCliExe <- findCardanoCliExecutable
      (skFile, vkFile) <- findKeys Game network
      gameAddress <- getVerificationKeyAddress vkFile network
      socketPath <- findSocketPath network

      -- find chess game script & address
      gameScriptFile <- findGameScriptFile network
      gameScriptAddress <- getScriptAddress gameScriptFile network

      -- retrieve current UTxO state
      utxo <- collectUTxO events cnx

      -- find collateral to submit play tx
      let collateral = findCollateral gameAddress utxo

      -- find current game state
      gameState@ChessGame{game, players} <-
        case extractGameState gameScriptAddress utxo of
          Left err ->
            throwIO $ UTxOError $ "Cannot extract game state from: " <> err
          Right game -> pure game

      (gameStateTxIn, gameScriptValue) <- case extractGameTxIn gameScriptAddress utxo of
        Left err -> do
          throwIO $ UTxOError $ "Cannot extract game value: " <> err
        Right v -> pure v

      -- compute next game state
      nextGame <- either (throwIO . InvalidMove) pure $ Chess.apply move game

      let nextState = ChessGame{players, game = nextGame}

      -- construct chess game inline datum
      gameDatumFile <- findDatumFile "game-state" nextState network
      moveRedeemerFile <- findDatumFile "chess-move" play network

      -- define transaction arguments
      let args =
            [ "--tx-in"
            , collateral
            , "--tx-in-collateral"
            , collateral
            , "--tx-in"
            , gameStateTxIn
            , "--tx-in-script-file"
            , gameScriptFile
            , "--tx-in-inline-datum-present"
            , "--tx-in-redeemer-file"
            , moveRedeemerFile
            , "--tx-in-execution-units"
            , "(500000000000,1000000000)"
            , "--tx-out"
            , gameScriptAddress <> " + " <> stringifyValue gameScriptValue
            , "--tx-out-inline-datum-file"
            , gameDatumFile
            , "--tx-out"
            , gameAddress <> "+ 8000000 lovelace" -- FIXME shoudl be value in collateral
            ]

      submitNewTx cnx args skFile

  endGame :: TVar IO (Seq (FromChain Chess Hydra)) -> Connection -> HeadId -> Value -> IO ()
  endGame events cnx headId utxo =
    try go >>= \case
      Left (InvalidGameTokens own their) -> putStrLn $ "Invalid game tokens, own: " <> show own <> ", their: " <> show their
      Left other -> putStrLn $ "Error building end game tx:  " <> show other
      Right{} -> pure ()
   where
    go = do
      -- retrieve needed tools and keys
      cardanoCliExe <- findCardanoCliExecutable
      (skFile, vkFile) <- findKeys Game network
      gameAddress <- getVerificationKeyAddress vkFile network
      socketPath <- findSocketPath network

      -- find chess game script & address
      gameScriptFile <- findGameScriptFile network
      gameScriptAddress <- getScriptAddress gameScriptFile network

      -- find ELO script & address
      (pkh, eloScriptFile) <- findEloScriptFile vkFile network
      eloScriptAddress <- getScriptAddress eloScriptFile network

      -- retrieve current UTxO state
      utxo <- collectUTxO events cnx

      -- find collateral to submit end game tx
      let collateral = findCollateral gameAddress utxo

      gameState <-
        case extractGameState gameScriptAddress utxo of
          Left err ->
            throwIO $ UTxOError $ "Cannot extract game state from: " <> err
          Right game -> pure game

      (gameStateTxIn, (adas, tokens)) <- case extractGameTxIn gameScriptAddress utxo of
        Left err -> do
          throwIO $ UTxOError $ "Cannot extract game value: " <> err
        Right v -> pure v

      let (own, their) = List.partition (\(_, pk, _) -> pk == pkh) tokens

      when (length own /= 1) $ throwIO $ InvalidGameTokens own their

      args <-
        if null their
          then do
            -- construct chess game inline datum
            endGameRedeemerFile <-
              -- TODO: find correct side to end
              findDatumFile "endgame" End network

            -- define transaction arguments
            pure $
              [ "--tx-in"
              , collateral
              , "--tx-in-collateral"
              , collateral
              , "--tx-in"
              , gameStateTxIn
              , "--tx-in-script-file"
              , gameScriptFile
              , "--tx-in-inline-datum-present"
              , "--tx-in-redeemer-file"
              , endGameRedeemerFile
              , "--tx-in-execution-units"
              , "(100000000000,1000000000)"
              , "--tx-out"
              , eloScriptAddress <> " + " <> stringifyValue (adas, own)
              , "--tx-out-inline-datum-file"
              , "1000" -- FIXME: should be some change in ELO rating
              , "--tx-out"
              , gameAddress <> "+ 8000000 lovelace" -- FIXME shoudl be value in collateral
              ]
          else do
            -- construct chess game inline datum
            gameDatumFile <- findDatumFile "game-state" gameState network
            endGameRedeemerFile <- findDatumFile "endgame" End network

            -- define transaction arguments
            pure $
              [ "--tx-in"
              , collateral
              , "--tx-in-collateral"
              , collateral
              , "--tx-in"
              , gameStateTxIn
              , "--tx-in-script-file"
              , gameScriptFile
              , "--tx-in-inline-datum-present"
              , "--tx-in-redeemer-file"
              , endGameRedeemerFile
              , "--tx-in-execution-units"
              , "(100000000000,1000000000)"
              , "--tx-out"
              , gameScriptAddress <> " + " <> stringifyValue (adas - 2000000, their)
              , "--tx-out-inline-datum-file"
              , gameDatumFile
              , "--tx-out"
              , eloScriptAddress <> " + " <> stringifyValue (20000000, own)
              , "--tx-out-inline-datum-file"
              , "1000" -- FIXME: should be some change in ELO rating
              , "--tx-out"
              , gameAddress <> "+ 8000000 lovelace" -- FIXME shoudl be value in collateral
              ]

      submitNewTx cnx args skFile

  makeValue :: String -> (String, String, Integer) -> String
  makeValue pid (_, pkh, _) = "1 " <> pid <.> pkh

  findCollateral :: String -> Value -> String
  findCollateral address utxo =
    maybe (error "Cannot find collateral for " <> address <> " from " <> asString utxo) id $
      case utxo of
        Object kv -> fmap (unpack . Key.toText . fst) $ List.find (findUTxOWithAddress address) (KeyMap.toList kv)
        _ -> Nothing

  findGameTokens ::
    String ->
    Value ->
    [ ( String -- txin
      , String -- pkh
      , Integer -- lovelace
      )
    ]
  findGameTokens pid utxo =
    case utxo of
      Object kv -> foldMap (findUTxOWithPolicyId pid) (KeyMap.toList kv)
      _ -> []

  makeGameInput :: (String, String, Integer) -> IO [String]
  makeGameInput (txin, pkh, _) = do
    eloScriptFile <- makeEloScriptFile pkh network

    pure $
      [ "--tx-in"
      , txin
      , "--tx-in-script-file"
      , eloScriptFile
      , "--tx-in-inline-datum-present"
      , "--tx-in-redeemer-value"
      , "[]"
      , "--tx-in-execution-units"
      , "(10000000,100000000)"
      ]

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
  collectUTxO :: TVar IO (Seq (FromChain g Hydra)) -> Connection -> IO Value
  collectUTxO events cnx = do
    WS.sendTextData cnx (encode GetUTxO)
    timeout
      600_000_000
      ( waitFor events $ \case
          OtherMessage (JsonContent txt) -> Just txt
          _ -> Nothing
      )
      >>= maybe (throwIO $ ServerException "Timeout (10m) waiting for GetUTxO") pure

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

data Request = Init | Close | Fanout | GetUTxO | NewTx Text
  deriving stock (Eq, Show, Generic)

instance ToJSON Request where
  toJSON = \case
    Init -> object ["tag" .= ("Init" :: Text)]
    Close -> object ["tag" .= ("Close" :: Text)]
    Fanout -> object ["tag" .= ("Fanout" :: Text)]
    GetUTxO -> object ["tag" .= ("GetUTxO" :: Text)]
    NewTx txCBOR ->
      object
        [ "tag" .= ("NewTx" :: Text)
        , "transaction" .= txCBOR
        ]

instance FromJSON Request where
  parseJSON = withObject "Request" $ \obj ->
    obj .: "tag" >>= \case
      ("Init" :: String) -> pure Init
      ("Close" :: String) -> pure Close
      ("Fanout" :: String) -> pure Close
      ("GetUTxO" :: String) -> pure GetUTxO
      ("NewTx" :: String) -> NewTx <$> obj .: "transaction"
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
  | TxValid {headId :: HeadId, transaction :: Value}
  | TxInvalid {headId :: HeadId, utxo :: Value, transaction :: Value, validationError :: Value}
  | SnapshotConfirmed {headId :: HeadId, snapshot :: Snapshot, signatures :: Value}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Snapshot = Snapshot {headId :: HeadId, utxo :: Value, confirmed :: [Text]}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON Snapshot where
  parseJSON = withObject "Snapshot" $ \obj ->
    Snapshot
      <$> (obj .: "headId")
      <*> (obj .: "utxo")
      <*> (obj .: "confirmedTransactions")

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
--   | TxExpired {headId :: HeadId, transaction :: tx}
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

asString :: (ToJSON a) => a -> String
asString = unpack . decodeUtf8 . LBS.toStrict . encode

extractGameToken :: String -> String -> Value -> Maybe String
extractGameToken pid pkh = \case
  Object kv -> foldMap (findUTxOWithValue pid pkh) (KeyMap.toList kv)
  _ -> Nothing

findUTxOWithAddress :: String -> (KeyMap.Key, Value) -> Bool
findUTxOWithAddress address (txin, txout) =
  case txout of
    Object kv ->
      case filter ((== fromString "address") . fst) $ KeyMap.toList kv of
        [(_, String addr)] | addr == pack address -> True
        _ -> False
    _ -> False

findUTxOWithPolicyId :: String -> (Aeson.Key, Value) -> GameTokens
findUTxOWithPolicyId pid (txin, txout) =
  case txout of
    Object kv ->
      case filter ((== fromString "value") . fst) $ KeyMap.toList kv of
        [(_, Object val)] ->
          case filter ((== fromString pid) . fst) $ KeyMap.toList val of
            [(_, Object toks)] -> fmap (\pkh -> (unpack $ Key.toText $ txin, unpack $ Key.toText $ pkh, 2000000)) $ KeyMap.keys toks
            _ -> []
        _ -> []
    _ -> []

findUTxOWithValue :: String -> String -> (KeyMap.Key, Value) -> Maybe String
findUTxOWithValue pid pkh (txin, txout) =
  case txout of
    Object kv ->
      if hasValueFor pid pkh kv
        then Just $ unpack $ Key.toText txin
        else Nothing
    _ -> Nothing

hasValueFor :: String -> String -> Aeson.Object -> Bool
hasValueFor pid pkh kv =
  case filter ((== fromString "value") . fst) $ KeyMap.toList kv of
    [(_, Object val)] ->
      case filter ((== fromString pid) . fst) $ KeyMap.toList val of
        [(_, Object tok)] ->
          case filter ((== fromString pkh) . fst) $ KeyMap.toList tok of
            [] -> False
            _ -> True
        _ -> False
    _ -> False

extractGameState :: String -> Value -> Either Text ChessGame
extractGameState address utxo =
  case utxo of
    Object kv ->
      case List.find (findUTxOWithAddress address) (KeyMap.toList kv) of
        Nothing -> Left $ "No output at address " <> pack address <> " for utxo"
        Just (_, txout) -> findGameState txout
    _ -> Left $ "Not an object: " <> (decodeUtf8 $ LBS.toStrict $ encode utxo)

extractGameTxIn :: String -> Value -> Either Text (String, (Integer, GameTokens))
extractGameTxIn address utxo =
  case utxo of
    Object kv ->
      case List.find (findUTxOWithAddress address) (KeyMap.toList kv) of
        Nothing -> Left $ "No output at address " <> pack address <> " for utxo"
        Just (txin, txout) -> (unpack $ Key.toText txin,) <$> extractValue txout
    _ -> Left $ "Not an object: " <> (decodeUtf8 $ LBS.toStrict $ encode utxo)

extractValue :: Value -> Either Text (Integer, GameTokens)
extractValue = \case
  Object kv ->
    case kv KeyMap.!? fromString "value" of
      Just (Object v) -> extractTokens v
      _ -> Left "No key 'inlineDatum'"
  _ -> Left "Not an object"

extractTokens :: Aeson.Object -> Either Text (Integer, GameTokens)
extractTokens kv = do
  adas :: Integer <- first ((<> " is not an integral number") . pack . show @Double) . floatingOrInteger =<< lovelace
  pure $ (adas, tokens)
 where
  lovelace = case kv KeyMap.!? fromString "lovelace" of
    Just (Number n) -> Right n
    _ -> Left "No key 'inlineDatum'"

  tokens = foldMap getTokens $ filter ((/= "lovelace") . fst) $ KeyMap.toList kv

  getTokens :: (Aeson.Key, Value) -> GameTokens
  getTokens (pid, v) =
    case v of
      Object toks -> foldMap (getToken pid) $ KeyMap.toList toks
      _ -> []

  getToken :: Aeson.Key -> (Aeson.Key, Value) -> GameTokens
  getToken pid (tok, Number val) =
    case floatingOrInteger @Double val of
      Left _ -> []
      Right i -> [(unpack $ Key.toText pid, unpack $ Key.toText tok, i)]
  getToken _ _ = []

stringifyValue :: (Integer, GameTokens) -> String
stringifyValue (adas, tokens) =
  show adas <> " lovelace + " <> concat (intersperse " + " (map stringify tokens))
 where
  stringify (pid, tok, val) = show val <> " " <> pid <.> tok

findGameState :: Value -> Either Text ChessGame
findGameState txout =
  extractInlineDatum txout >>= fromJSONDatum

extractInlineDatum :: Value -> Either Text Value
extractInlineDatum value =
  case value of
    Object kv ->
      case kv KeyMap.!? fromString "inlineDatum" of
        Just v -> Right v
        _ -> Left $ "No key 'inlineDatum' in " <> (decodeUtf8 $ LBS.toStrict $ encode value)
    _ -> Left $ "Not an object" <> (decodeUtf8 $ LBS.toStrict $ encode value)
