{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module BlackJack.Game where

import Data.List (nub, sortBy)
import Data.Map (Map, size)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import System.Random (StdGen, Uniform, mkStdGen, uniform)
import Test.QuickCheck (Arbitrary (..), elements, suchThat)

possibleActions :: BlackJack -> [Play]
possibleActions Setup{numPlayers, initialBets} =
  let players = playerIds 1 numPlayers
      missingBet p = maybe [Bet p] (const []) $ Map.lookup p initialBets
   in foldMap missingBet players
possibleActions game@BlackJack{next} = possibleActionsFor next game

possibleActionsFor :: PlayerId -> BlackJack -> [Play]
possibleActionsFor player Setup{initialBets} =
  case Map.lookup player initialBets of
    Just{} -> []
    Nothing -> [Bet player]
possibleActionsFor player BlackJack{dealerHand, players} =
  case player of
    PlayerId{} -> case Map.lookup player players of
      Just Playing{hand} -> actionsFor player hand
      Just Standing{} -> [Stand player]
      _ -> error $ "Invalid player number " <> show player
    Dealer -> dealerActions dealerHand

dealerActions :: DealerHand -> [Play]
dealerActions dealerHand =
  let value = minimum $ dealerValues dealerHand
   in if value < 17 then [Hit Dealer] else [Stand Dealer]

dealerValues :: DealerHand -> [Int]
dealerValues (DealerHand (Hidden c, cards)) = handValues (c : cards)

runPlays :: [Play] -> BlackJack -> Outcome
runPlays plays initialGame =
  foldr doPlay (GameContinue initialGame) plays
 where
  doPlay :: Play -> Outcome -> Outcome
  doPlay p (GameContinue game) = play game p
  doPlay _ outcome = outcome

play :: BlackJack -> Play -> Outcome
play game@BlackJack{} (Hit player) =
  case player of
    Dealer ->
      GameContinue $ dealOneCardToDealer game
    PlayerId{} ->
      GameContinue $ dealOneCardTo player game
play game@BlackJack{next, dealerHand, players} (Stand player) =
  case player of
    Dealer ->
      GameEnds (reveal dealerHand) $ payoffs dealerHand players
    PlayerId{} ->
      let game' =
            game
              { players = Map.update (Just . stand) player players
              }
       in GameContinue $ game'{next = nextPlayer next players}
play BlackJack{dealerHand} Quit = GameEnds (reveal dealerHand) mempty
play game@BlackJack{next, players} _ = GameContinue $ game{next = nextPlayer next players}
play Setup{} Quit = GameEnds [] mempty
play setup@Setup{numPlayers, seed, initialBets} (Bet p) =
  let newBets = Map.insert p 100 initialBets
   in GameContinue $
        if size newBets == numPlayers
          then dealInitialCards newBets seed
          else setup{initialBets = newBets}
play Setup{} p = error $ "invalid play in setup stage: " <> show p

payoffs :: DealerHand -> Map PlayerId Player -> Map PlayerId Int
payoffs dealerHand = Map.map (payoff dealerHand)

payoff :: DealerHand -> Player -> Int
payoff (reveal -> dealerHand) = \case
  (Playing cas n) -> computePayoff cas n
  (Standing cas n) -> computePayoff cas n
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

dealInitialCards :: Map PlayerId Int -> Int -> BlackJack
dealInitialCards bets seed =
  let (seed', playerHands) = foldr dealCardsToPlayer (mkStdGen seed, mempty) $ Map.keys bets
      (seed'', cs) = dealCards 2 seed'
      player p card =
        Playing card $ fromMaybe (error "cannot build initial players") $ Map.lookup p bets
   in BlackJack
        { numPlayers = length bets
        , dealerHand = mkDealerHand cs
        , next = 1
        , gen = seed''
        , players = Map.mapWithKey player playerHands
        }

dealCardsToPlayer :: PlayerId -> (StdGen, Map PlayerId [Card]) -> (StdGen, Map PlayerId [Card])
dealCardsToPlayer player (seed, playerHands) =
  let (seed', cards) = dealCards 2 seed
   in (seed', Map.insert player cards playerHands)

dealCards :: Int -> StdGen -> (StdGen, [Card])
dealCards num g =
  foldr (const dealOneCard) (g, []) [1 .. num]

dealOneCard :: (StdGen, [Card]) -> (StdGen, [Card])
dealOneCard (g, cards) =
  let (card, g') = uniform g
   in (g', card : cards)

data Play
  = Quit
  | Bet PlayerId
  | Stand PlayerId
  | Hit PlayerId
  deriving (Eq, Ord, Show)

isHit :: Play -> Bool
isHit (Hit _) = True
isHit _ = False

forPlayer :: Play -> PlayerId
forPlayer Quit = 0 -- TODO: does not make sense
forPlayer (Bet n) = n
forPlayer (Stand n) = n
forPlayer (Hit n) = n

nextPlayer :: PlayerId -> Map PlayerId Player -> PlayerId
nextPlayer player players =
  case player of
    PlayerId p
      | p < length players -> PlayerId $ succ p
      | otherwise -> Dealer
    Dealer -> PlayerId 1

playerIds :: Int -> Int -> [PlayerId]
playerIds lb ub = PlayerId <$> [lb .. ub]

data Outcome
  = GameEnds [Card] (Map PlayerId Int) -- bets
  | GameContinue BlackJack
  deriving (Eq, Show)

isEndGame :: Outcome -> Bool
isEndGame GameEnds{} = True
isEndGame GameContinue{} = False

data Color = Heart | Spade | Diamond | Club
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Uniform)

instance Arbitrary Color where
  arbitrary = elements [Heart, Spade, Diamond, Club]

data Face
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving stock (Eq, Ord, Enum, Show, Generic)
  deriving anyclass (Uniform)

instance Arbitrary Face where
  arbitrary = elements $ enumFromTo Two Ace
  shrink face = filter (< face) $ enumFromTo Two Ace

data Card = Card {color :: Color, face :: Face}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Uniform)

instance Arbitrary Card where
  arbitrary = Card <$> arbitrary <*> arbitrary
  shrink (Card color face) = Card color <$> shrink face

handValues :: [Card] -> [Int]
handValues = nub . foldr cardValues []

cardValues :: Card -> [Int] -> [Int]
cardValues card [] = cardValue card
cardValues card values = [vals + v | vals <- values, v <- cardValue card]

cardValue :: Card -> [Int]
cardValue Card{face = Two} = [2]
cardValue Card{face = Three} = [3]
cardValue Card{face = Four} = [4]
cardValue Card{face = Five} = [5]
cardValue Card{face = Six} = [6]
cardValue Card{face = Seven} = [7]
cardValue Card{face = Eight} = [8]
cardValue Card{face = Nine} = [9]
cardValue Card{face = Ten} = [10]
cardValue Card{face = Jack} = [10]
cardValue Card{face = Queen} = [10]
cardValue Card{face = King} = [10]
cardValue Card{face = Ace} = [1, 11]

isBlackJack :: [Card] -> Bool
isBlackJack cards = length cards == 2 && 21 `elem` handValues cards

newtype DealerHand = DealerHand (Hidden Card, [Card])
  deriving (Eq, Show)

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

reveal :: DealerHand -> [Card]
reveal (DealerHand (Hidden c, cs)) = c : cs

actionsFor :: PlayerId -> [Card] -> [Play]
actionsFor player hand
  | minimum (handValues hand) >= 21 = [Stand player]
  | otherwise = [Hit player, Stand player]

newtype Hidden c = Hidden c
  deriving (Eq, Show)

data PlayerId
  = PlayerId {playerId :: Int}
  | Dealer
  deriving stock (Eq, Ord, Show)

-- NOTE: Only used to simplify definition for numbers
instance Num PlayerId where
  fromInteger = PlayerId . fromInteger
  (+) = error "undefined"
  (*) = error "undefined"
  abs = error "undefined"
  signum = error "undefined"
  negate = error "undefined"

data Player
  = Playing {hand :: [Card], bets :: Int}
  | Standing {hand :: [Card], bets :: Int}
  deriving (Eq, Show)

stand :: Player -> Player
stand Playing{hand, bets} = Standing{hand, bets}
stand p = p

isStanding :: Player -> Bool
isStanding Playing{} = False
isStanding Standing{} = True

data BlackJack
  = Setup
      { numPlayers :: Int
      , seed :: Int
      , initialBets :: Map PlayerId Int
      }
  | BlackJack
      { numPlayers :: Int
      , gen :: StdGen
      , next :: PlayerId
      , dealerHand :: DealerHand
      , players :: Map PlayerId Player
      }
  deriving (Eq, Show)

playerHand :: PlayerId -> BlackJack -> [Card]
playerHand _ Setup{} = []
playerHand p BlackJack{players} = maybe [] hand $ Map.lookup p players

dealOneCardTo :: PlayerId -> BlackJack -> BlackJack
dealOneCardTo player game@BlackJack{gen, players} =
  let hand = playerHand player game
      (gen', newHand) = dealOneCard (gen, hand)
   in game
        { gen = gen'
        , players = Map.update (\p@Playing{} -> Just p{hand = newHand}) player players
        }
dealOneCardTo _ Setup{} = error "should never happen"

dealOneCardToDealer :: BlackJack -> BlackJack
dealOneCardToDealer game@BlackJack{gen, dealerHand = (DealerHand (h, cs))} =
  let (gen', cs') = dealOneCard (gen, cs)
   in game{gen = gen', dealerHand = DealerHand (h, cs')}
dealOneCardToDealer Setup{} = error "should never happen"

newGame :: Int -> Int -> BlackJack
newGame numPlayers seed = Setup numPlayers seed mempty

mkGame :: Int -> PlayerId -> Int -> [(PlayerId, (Int, [Card]))] -> BlackJack
mkGame numPlayers next seed hands =
  let (seed', cs) = dealCards 2 (mkStdGen seed)
   in BlackJack
        { numPlayers
        , gen = seed'
        , next
        , dealerHand = mkDealerHand cs
        , players = Map.fromList $ mkPlayer <$> hands
        }

mkPlayer :: (PlayerId, (Int, [Card])) -> (PlayerId, Player)
mkPlayer (player, (bet, cards)) = (player, Playing cards bet)
