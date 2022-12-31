{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module HttpServer where

import BlackJack.Game (
  Outcome (GameContinue, GameEnds),
  Payoffs,
  Play (Bet),
  PlayerId (Dealer, PlayerId),
  decodePlayerId,
  forPlayer,
  newGame,
  play,
  possibleActions,
 )
import BlackJack.Server (
  FromChain (..),
  HeadId (..),
  Indexed (..),
 )
import BlackJack.Server.Mock (MockChain, MockCoin (MockCoin), MockParty (Party), pid)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Monad (replicateM)
import Data.Aeson (FromJSON, ToJSON, Value (Object), eitherDecode, encode, toJSON)
import Data.Aeson.KeyMap (insert)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import Data.Function ((&))
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq, (<|), (|>))
import qualified Data.Sequence as Seq
import Data.String (IsString (fromString))
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (getCurrentTime)
import qualified GHC.Clock
import GHC.Generics (Generic)
import Network.HTTP.Types (badRequest400, methodNotAllowed405, notFound404, ok200, statusCode)
import Network.Wai (
  Application,
  Middleware,
  pathInfo,
  requestMethod,
  responseLBS,
  responseStatus,
  strictRequestBody,
 )
import qualified Network.Wai.Handler.Warp as Warp
import System.Random.Stateful (applyAtomicGen, globalStdGen, uniform, uniformR)
import Prelude hiding (head)

data HttpLog
  = HttpServerListening {host :: Text, port :: Int}
  | HttpRequest {path :: Text, method :: Text}
  | HttpResponse {status :: Int, time :: Double}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

httpServer :: String -> Int -> IO ()
httpServer host port =
  newTVarIO (Chain mempty mempty mempty) >>= Warp.runSettings settings . logRequest . app
 where
  logRequest :: Middleware
  logRequest k req send = do
    start <- GHC.Clock.getMonotonicTime
    doLog
      HttpRequest
        { path = "/" <> Text.intercalate "/" (pathInfo req)
        , method = decodeUtf8 (requestMethod req)
        }
    k req $ \res -> do
      result <- send res
      end <- GHC.Clock.getMonotonicTime
      let time = (end - start) * 1_000
      let status = statusCode $ responseStatus res
      doLog HttpResponse{status, time}
      pure result

  settings =
    Warp.defaultSettings
      & Warp.setPort port
      & Warp.setHost (fromString host)
      & Warp.setServerName "mock-chain"
      & Warp.setBeforeMainLoop (doLog HttpServerListening{host = pack host, port})

data HeadState
  = Initialising
      { peers :: [MockParty]
      , committed :: Map Text Int
      }
  | Open
      { peers :: [MockParty]
      , gains :: Map Text Int
      , outcome :: Outcome
      }
  | Closing
      { peers :: [MockParty]
      , payoffs :: Map Text Int
      }
  deriving (Eq, Show)

data Chain = Chain
  { knownHosts :: Map Text MockParty
  , heads :: Map HeadId HeadState
  , events :: Seq (FromChain MockChain)
  }
  deriving stock (Eq, Show)

app :: TVar Chain -> Application
app state req send =
  route (requestMethod req) (pathInfo req)
 where
  route "POST" ["connect", hostId] = handleConnect hostId
  route "POST" ["init"] = handleInit
  route "POST" ["commit", headId, partyId, amount] = handleCommit (HeadId headId) partyId (readNumber amount)
  route "POST" ["play", headId, partyId, num] = handlePlay (HeadId headId) partyId (readNumber num)
  route "POST" ["start", headId] = handleRestart (HeadId headId)
  route "POST" ["close", headId] = handleClose (HeadId headId)
  route "GET" ["events", idx, num] = handleEvents (readNumber idx) (readNumber num)
  route "GET" _ = send $ responseLBS notFound404 [] ""
  route _ _ = send $ responseLBS methodNotAllowed405 [] ""

  handleConnect hostId = do
    decoded <- eitherDecode <$> strictRequestBody req
    case decoded of
      Left err -> send $ responseLBS badRequest400 [] ("Invalid host info body: " <> LBS.fromStrict (encodeUtf8 $ pack err))
      Right hostInfo -> do
        atomically $ modifyTVar' state $ \chain@Chain{knownHosts} -> chain{knownHosts = Map.insert hostId hostInfo knownHosts}
        send $ responseLBS ok200 [] ""

  handleInit = do
    decoded <- eitherDecode <$> strictRequestBody req
    case decoded of
      Left err -> send $ responseLBS badRequest400 [] ("Invalid peers body: " <> LBS.fromStrict (encodeUtf8 $ pack err))
      Right (peers :: [Text]) -> do
        newHeadId <- mkNewHeadId
        atomically $ do
          chain@Chain{heads, knownHosts, events} <- readTVar state
          let headPeers = mapMaybe (`Map.lookup` knownHosts) peers
              chain' =
                chain
                  { heads = Map.insert newHeadId (Initialising headPeers mempty) heads
                  , events = events |> HeadCreated newHeadId headPeers
                  }
          writeTVar state chain'
        send $ responseLBS ok200 [] $ encode newHeadId

  handleCommit head@(HeadId hid) partyId amount = do
    let seed = fromInteger $ readNumber $ "0x" <> hid
    res <- atomically $ do
      chain@Chain{heads, events} <- readTVar state
      case Map.lookup head heads of
        Nothing -> pure $ Left $ "cannot find headId " <> hid
        Just h@Initialising{} -> do
          let party = List.find (\p -> pid p == partyId) $ peers h
          case party of
            Nothing -> pure $ Left $ "cannot find party " <> partyId
            Just p@Party{} -> do
              let head' = h{committed = Map.insert partyId amount (committed h)}
                  game = newGame (length (peers head') - 1) seed
                  plays = possibleActions game
                  openHead =
                    if length (committed head') == length (peers head')
                      then HeadOpened head <| GameStarted head game plays <| mempty
                      else mempty
                  newEvents = FundCommitted @MockChain head p (MockCoin $ fromIntegral amount) <| openHead
                  head'' =
                    if null openHead
                      then head'
                      else
                        Open
                          { peers = peers head'
                          , outcome = GameContinue game
                          , gains = committed head'
                          }
                  chain' =
                    chain
                      { heads = Map.insert head head'' heads
                      , events = events <> newEvents
                      }
              writeTVar state chain'
              pure $ Right ()
        Just _ -> pure $ Right () -- TODO: really ignore?
    send $ responseLBS (either (const badRequest400) (const ok200) res) [] ""

  handlePlay head@(HeadId hid) partyId num = do
    res <- atomically $ do
      chain@Chain{heads, events} <- readTVar state
      case Map.lookup head heads of
        Nothing -> pure $ Left $ "cannot find headId " <> hid
        Just Open{peers, gains, outcome = GameContinue game} -> do
          let party = List.findIndex (\p -> pid p == partyId) peers
          case party of
            Nothing -> pure $ Left $ "cannot find party " <> partyId
            Just numPlayer -> do
              let playerId = decodePlayerId numPlayer
                  acts = possibleActions game
              if num >= length acts
                then pure $ Left $ "invalid play for player " <> partyId
                else do
                  let p = acts !! num
                  if forPlayer p == playerId
                    then do
                      let outcome = play game p
                          (event, newState) =
                            case outcome of
                              GameEnds dealerCards payoffs ->
                                let gains' = applyGains gains peers payoffs
                                 in ( GameEnded head dealerCards payoffs gains'
                                    , Open{peers, gains = gains', outcome}
                                    )
                              GameContinue game' ->
                                let dealerId = pid $ List.head peers
                                    open = case p of
                                      Bet _ ->
                                        Open
                                          { outcome
                                          , peers
                                          , gains = Map.adjust (+ 100) dealerId $ Map.adjust (+ (-100)) partyId gains
                                          }
                                      _ -> Open{outcome, peers, gains}
                                 in (GameChanged head game' (possibleActions game'), open)

                          chain' =
                            chain
                              { heads = Map.insert head newState heads
                              , events = events |> event
                              }

                      writeTVar state chain'
                      pure $ Right ()
                    else pure $ Left $ "invalid play " <> pack (show p) <> " for player " <> partyId
        Just _ -> pure $ Left $ pack $ "game with id " <> show hid <> " is not opened"
    send $ responseLBS (either (const badRequest400) (const ok200) res) [] ""

  handleRestart head@(HeadId hid) = do
    seed <- randomSeed -- TODO get the seed from the previous game
    res <- atomically $ do
      chain@Chain{heads, events} <- readTVar state
      case Map.lookup head heads of
        Nothing -> pure $ Left $ "cannot find headId " <> hid
        -- TODO do something with payoffs
        Just Open{peers, gains} -> do
          let game = newGame (length peers - 1) seed
              acts = possibleActions game
              event = GameStarted head game acts
              chain' = chain{heads = Map.insert head (Open{peers, outcome = GameContinue game, gains}) heads, events = events |> event}
          writeTVar state chain'
          pure $ Right ()
        Just _ -> pure $ Left $ pack $ "game with id " <> show hid <> " is not finished"
    send $ responseLBS (either (const badRequest400) (const ok200) res) [] ""

  -- TODO what happens with contestation?
  -- we close with the latest gains which includes the bets in the current game,
  -- this is obviously a great way for the dealer to gain money!
  handleClose head@(HeadId hid) = do
    res <- atomically $ do
      chain@Chain{heads, events} <- readTVar state
      case Map.lookup head heads of
        Nothing -> pure $ Left $ "cannot find headId " <> hid
        Just Open{peers, gains} -> do
          let event = HeadClosed head gains
              chain' =
                chain
                  { heads = Map.insert head (Closing{peers, payoffs = gains}) heads
                  , events = events |> event
                  }
          writeTVar state chain'
          pure $ Right ()
        Just _ -> pure $ Left $ pack $ "game with id " <> show hid <> " is not running"
    send $ responseLBS (either (const badRequest400) (const ok200) res) [] ""

  handleEvents idx num = do
    Chain{events} <- readTVarIO state
    let indexed =
          Indexed
            { lastIndex = fromIntegral $ length events
            , events = toList $ Seq.take num $ Seq.drop idx events
            }
    send $ responseLBS ok200 [] $ encode indexed

applyGains :: Map Text Int -> [MockParty] -> Payoffs -> Map Text Int
applyGains gains peers payoffs =
  let won = sum payoffs
      playerToParty =
        pid . \case
          (PlayerId n) -> peers !! n
          Dealer -> List.head peers
      payoffsToParty = Map.mapKeys playerToParty payoffs
   in Map.adjust (+ (- won)) (playerToParty Dealer) $
        Map.unionWith (+) gains payoffsToParty

readNumber :: (Read a, Num a) => Text -> a
readNumber v =
  case reads (unpack v) of
    [(a, [])] -> a
    _ -> error $ "fail to read from " <> unpack v

randomSeed :: IO Int
randomSeed = applyAtomicGen uniform globalStdGen

mkNewHeadId :: IO HeadId
mkNewHeadId = HeadId . pack . fmap toChar <$> replicateM 32 randomNibble
 where
  randomNibble = applyAtomicGen (uniformR (0x0, 0xF)) globalStdGen
  toChar :: Int -> Char
  toChar 0 = '0'
  toChar 1 = '1'
  toChar 2 = '2'
  toChar 3 = '3'
  toChar 4 = '4'
  toChar 5 = '5'
  toChar 6 = '6'
  toChar 7 = '7'
  toChar 8 = '8'
  toChar 9 = '9'
  toChar 10 = 'a'
  toChar 11 = 'b'
  toChar 12 = 'c'
  toChar 13 = 'd'
  toChar 14 = 'e'
  toChar 15 = 'f'
  toChar _ = error "unexpected value"

doLog :: (ToJSON a) => a -> IO ()
doLog l = do
  ts <- getCurrentTime
  BS.putStr $ LBS.toStrict (logEntry ts <> "\n")
 where
  logEntry ts =
    let val = toJSON l
     in encode $ case val of
          Object obj -> Object $ insert "timestamp" (toJSON ts) obj
          v -> v
