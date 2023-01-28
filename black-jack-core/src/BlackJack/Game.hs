{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module BlackJack.Game (
  module BlackJack.Contract.Game,
  module BlackJack.Game,
) where

import BlackJack.Contract.Game (
  BlackJack (..),
  Card (..),
  Color (..),
  DealerHand (..),
  Face (..),
  Hidden (..),
  Outcome (..),
  Play (..),
  Player (..),
  PlayerId (..),
  actionsFor,
  handValues,
  hitting,
  isBlackJack,
  isHitting,
  reveal,
  standing,
 )
import Control.Monad (foldM)
import Control.Monad.State (State, gets, put)
import Data.Aeson.KeyMap ()
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import qualified PlutusTx.AssocMap as AssocMap
import System.Random (StdGen, uniform)
import Test.QuickCheck (Arbitrary (..), elements, suchThat)

dealerValues :: DealerHand -> [Integer]
dealerValues (DealerHand (Hidden c, cards)) = handValues (c : cards)
dealerValues (DealerHand (None, _)) = error "should not happen"

runPlays :: [Play] -> BlackJack -> State StdGen Outcome
runPlays plays initialGame =
  foldM doPlay (GameContinue initialGame) plays
 where
  doPlay :: Outcome -> Play -> State StdGen Outcome
  doPlay (GameContinue game) p = play game p
  doPlay outcome _ = pure outcome

play :: BlackJack -> Play -> State StdGen Outcome
play game@BlackJack{players} (Hit player) =
  case player of
    Dealer ->
      GameContinue <$> dealOneCardToDealer game
    PlayerId{} ->
      pure $ GameContinue game{next = Dealer, players = assocMapUpdate (Just . hitting) player players}
play game@BlackJack{next, dealerHand, players} (Stand player) =
  case player of
    Dealer ->
      pure $ GameEnds (reveal dealerHand) $ payoffs dealerHand players
    PlayerId{} ->
      let game' =
            game
              { players = assocMapUpdate (Just . standing) player players
              }
       in pure $ GameContinue $ game'{next = nextPlayer next players}
play game@BlackJack{} (DealCard player) =
  GameContinue <$> dealOneCardTo player game{next = player}
play BlackJack{dealerHand} Quit =
  pure $ GameEnds (reveal dealerHand) (AssocMap.fromList [])
play game@BlackJack{next, players} _ =
  pure $ GameContinue $ game{next = nextPlayer next players}
play Setup{} Quit =
  pure $ GameEnds [] (AssocMap.fromList [])
play setup@Setup{numPlayers, initialBets} (Bet p) =
  let newBets = AssocMap.insert p 100 initialBets
   in GameContinue
        <$> if fromIntegral (length $ AssocMap.elems newBets) == numPlayers
          then dealInitialCards newBets
          else pure setup{initialBets = newBets}
play Setup{} p = error $ "invalid play in setup stage: " <> show p

assocMapUpdate :: Eq k => (a -> Maybe a) -> k -> AssocMap.Map k a -> AssocMap.Map k a
assocMapUpdate f k = AssocMap.mapWithKey update
 where
  update k' a
    | k == k' = fromMaybe a (f a)
    | otherwise = a


assocMapToList :: AssocMap.Map k v -> [(k, v)]
assocMapToList = AssocMap.toList

deletePlayer :: PlayerId -> AssocMap.Map PlayerId a -> AssocMap.Map PlayerId a
deletePlayer = AssocMap.delete

mapLength :: AssocMap.Map k a -> Int
mapLength = length . AssocMap.toList

payoffs :: DealerHand -> AssocMap.Map PlayerId Player -> AssocMap.Map PlayerId Integer
payoffs dealerHand = AssocMap.mapWithKey (const $ payoff dealerHand)

payoff :: DealerHand -> Player -> Integer
payoff (reveal -> dealerHand) = \case
  (Playing cas n) -> computePayoff cas n
  (Standing cas n) -> computePayoff cas n
  (Hitting cas n) -> computePayoff cas n
 where
  computePayoff cas n =
    case result cas dealerHand of
      WinBlackJack -> n * 5 `div` 2
      Win -> n * 2
      Tie -> n
      Lose -> 0

data Result = WinBlackJack | Win | Tie | Lose
  deriving stock (Eq, Show)

result :: [Card] -> [Card] -> Result
result a b =
  let score = sortBy (flip compare) . filter (<= 21) . handValues
      scoreA = score a
      scoreB = score b
   in case compare scoreA scoreB of
        GT ->
          if isBlackJack a
            then WinBlackJack
            else Win
        EQ ->
          if
              | isBlackJack a -> WinBlackJack
              | null scoreA -> Lose
              | otherwise -> Tie
        LT -> Lose

dealInitialCards :: AssocMap.Map PlayerId Integer -> State StdGen BlackJack
dealInitialCards bets = do
  playerHands <- foldM (flip dealCardsToPlayer) (AssocMap.fromList []) $ AssocMap.keys bets
  cs <- dealCards 2
  let player p card =
        Playing card $ fromMaybe (error "cannot build initial players") $ AssocMap.lookup p bets
  pure $
    BlackJack
      { numPlayers = fromIntegral (length $ AssocMap.toList bets)
      , dealerHand = mkDealerHand cs
      , next = 1
      , players = AssocMap.mapWithKey player playerHands
      }

dealCardsToPlayer :: PlayerId -> AssocMap.Map PlayerId [Card] -> State StdGen (AssocMap.Map PlayerId [Card])
dealCardsToPlayer player playerHands = do
  cards <- dealCards 2
  pure $ AssocMap.insert player cards playerHands

dealCards :: Integer -> State StdGen [Card]
dealCards num =
  foldM (flip $ const dealOneCard) [] [1 .. num]

dealOneCard :: [Card] -> State StdGen [Card]
dealOneCard cards = do
  (card, g') <- gets uniform
  put g'
  pure $ card : cards

nextPlayer :: PlayerId -> AssocMap.Map PlayerId Player -> PlayerId
nextPlayer player players =
  case player of
    PlayerId p
      | p < fromIntegral (length $ AssocMap.toList players) -> PlayerId $ succ p
      | otherwise -> Dealer
    Dealer -> PlayerId 1

playerIds :: Integer -> Integer -> [PlayerId]
playerIds lb ub = PlayerId <$> [lb .. ub]

instance Arbitrary Color where
  arbitrary = elements [Heart, Spade, Diamond, Club]

instance Arbitrary Face where
  arbitrary = elements $ enumFromTo Two Ace
  shrink face = filter (< face) $ enumFromTo Two Ace

instance Arbitrary Card where
  arbitrary = Card <$> arbitrary <*> arbitrary
  shrink (Card color face) = Card color <$> shrink face

instance Arbitrary DealerHand where
  arbitrary =
    arbitrary >>= fmap mkDealerHand . go . (: [])
   where
    go (c : cs)
      | any (<= 21) (handValues (c : cs)) = do
        c' <- arbitrary `suchThat` \c'' -> not (isBlackJack $ c'' : c : cs)
        go (c' : c : cs)
      | otherwise = pure cs
    go [] = error "should not happen"

-- FIXME: partial function
mkDealerHand :: [Card] -> DealerHand
mkDealerHand (c : cs) = DealerHand (Hidden c, cs)
mkDealerHand _ = error "dealer hand needs at least one card"

decodePlayerId :: Integer -> PlayerId
decodePlayerId 0 = Dealer
decodePlayerId n = PlayerId n

-- NOTE: Only used to simplify definition for numbers
instance Num PlayerId where
  fromInteger = PlayerId . fromInteger
  (+) = error "undefined"
  (*) = error "undefined"
  abs = error "undefined"
  signum = error "undefined"
  negate = error "undefined"

playerHand :: PlayerId -> BlackJack -> [Card]
playerHand _ Setup{} = []
playerHand p BlackJack{players} = maybe [] hand $ AssocMap.lookup p players

dealOneCardTo :: PlayerId -> BlackJack -> State StdGen BlackJack
dealOneCardTo player game@BlackJack{players} = do
  let hand = playerHand player game
  newHand <- dealOneCard hand
  pure $
    game
      { players = assocMapUpdate (\p@Hitting{} -> Just p{hand = newHand}) player players
      }
dealOneCardTo _ _ = error "should never happen"

dealOneCardToDealer :: BlackJack -> State StdGen BlackJack
dealOneCardToDealer game@BlackJack{dealerHand = (DealerHand (h, cs))} = do
  cs' <- dealOneCard cs
  pure $ game{dealerHand = DealerHand (h, cs')}
dealOneCardToDealer _ = error "should never happen"

newGame :: Integer -> BlackJack
newGame numPlayers = Setup numPlayers (AssocMap.fromList [])

mkGame :: Integer -> PlayerId -> [(PlayerId, (Integer, [Card]))] -> State StdGen BlackJack
mkGame numPlayers next hands = do
  cs <- dealCards 2
  pure $
    BlackJack
      { numPlayers
      , next
      , dealerHand = mkDealerHand cs
      , players = AssocMap.fromList $ mkPlayer <$> hands
      }

mkPlayer :: (PlayerId, (Integer, [Card])) -> (PlayerId, Player)
mkPlayer (player, (bet, cards)) = (player, Playing cards bet)
