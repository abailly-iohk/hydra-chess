{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialize -fdefer-type-errors #-}

-- | Plutus data structures and code for running BlackJack game on-chain.
module BlackJack.Contract.Game where

import PlutusTx.Prelude

import Control.Monad (fail, (>=>))
import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  withArray,
  withText,
 )
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import qualified PlutusTx
import PlutusTx.AssocMap (Map)
import qualified PlutusTx.AssocMap as Map
import System.Random (Uniform)
import qualified Prelude as Haskell
import Data.Functor ((<&>))

data PlayerId
  = PlayerId {playerId :: Integer}
  | Dealer
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''PlayerId

instance Eq PlayerId where
  {-# INLINEABLE (==) #-}
  (PlayerId n) == (PlayerId i) = n == i
  Dealer == Dealer = True
  _ == _ = False

decodePlayerId :: Integer -> PlayerId
decodePlayerId 0 = Dealer
decodePlayerId n = PlayerId n

data Play
  = Quit
  | Bet PlayerId
  | DealCard PlayerId -- contrary to other constructors this says to which player the card should be dealt
  | Stand PlayerId
  | Hit PlayerId
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Eq Play where
  {-# INLINEABLE (==) #-}
  Quit == Quit = True
  (Bet pi) == (Bet pi') = pi == pi'
  (DealCard pi) == (DealCard pi') = pi == pi'
  (Stand pi) == (Stand pi') = pi == pi'
  (Hit pi) == (Hit pi') = pi == pi'
  _ == _ = False

PlutusTx.unstableMakeIsData ''Play

isHit :: Play -> Bool
isHit (Hit _) = True
isHit _ = False

forPlayer :: Play -> PlayerId
forPlayer Quit = PlayerId 0 -- TODO: does not make sense
forPlayer DealCard{} = Dealer
forPlayer (Bet n) = n
forPlayer (Stand n) = n
forPlayer (Hit n) = n

data Color = Heart | Spade | Diamond | Club
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (Uniform)

PlutusTx.unstableMakeIsData ''Color

instance ToJSON Color where
  toJSON = \case
    Heart -> "\x2661"
    Spade -> "\x2660"
    Diamond -> "\x2662"
    Club -> "\x2663"

instance FromJSON Color where
  parseJSON = \case
    "\x2661" -> Haskell.pure Heart
    "\x2660" -> Haskell.pure Spade
    "\x2662" -> Haskell.pure Diamond
    "\x2663" -> Haskell.pure Club
    other -> fail $ "unknown color " <> Haskell.show other

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
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Enum, Haskell.Show, Generic)
  deriving anyclass (Uniform)

PlutusTx.unstableMakeIsData ''Face

instance ToJSON Face where
  toJSON = \case
    Two -> "2"
    Three -> "3"
    Four -> "4"
    Five -> "5"
    Six -> "6"
    Seven -> "7"
    Eight -> "8"
    Nine -> "9"
    Ten -> "10"
    Jack -> "J"
    Queen -> "Q"
    King -> "K"
    Ace -> "A"

instance FromJSON Face where
  parseJSON = withText "Face" $ \case
    "2" -> Haskell.pure Two
    "3" -> Haskell.pure Three
    "4" -> Haskell.pure Four
    "5" -> Haskell.pure Five
    "6" -> Haskell.pure Six
    "7" -> Haskell.pure Seven
    "8" -> Haskell.pure Eight
    "9" -> Haskell.pure Nine
    "10" -> Haskell.pure Ten
    "J" -> Haskell.pure Jack
    "Q" -> Haskell.pure Queen
    "K" -> Haskell.pure King
    "A" -> Haskell.pure Ace
    other -> fail $ "unknown face " <> Haskell.show other

data Card = Card {color :: Color, face :: Face}
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (Uniform, ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''Card

handValues :: [Card] -> [Integer]
handValues = nub . foldr cardValues []
{-# INLINEABLE handValues #-}

cardValues :: Card -> [Integer] -> [Integer]
cardValues card [] = cardValue card
cardValues card values = foldr (addCardValue $ cardValue card) [] values
 where
  addCardValue :: [Integer] -> Integer -> [Integer] -> [Integer]
  addCardValue vals v = (map (+ v) vals <>)
{-# INLINEABLE cardValues #-}

cardValue :: Card -> [Integer]
cardValue Card{face} = case face of
  Two -> [2]
  Three -> [3]
  Four -> [4]
  Five -> [5]
  Six -> [6]
  Seven -> [7]
  Eight -> [8]
  Nine -> [9]
  Ten -> [10]
  Jack -> [10]
  Queen -> [10]
  King -> [10]
  Ace -> [1, 11]
{-# INLINEABLE cardValue #-}

isBlackJack :: [Card] -> Bool
isBlackJack cards = length cards == 2 && 21 `elem` handValues cards
{-# INLINEABLE isBlackJack #-}

data Hidden c = Hidden c | None
  deriving (Haskell.Eq, Haskell.Show)

PlutusTx.unstableMakeIsData ''Hidden

newtype DealerHand = DealerHand (Hidden Card, [Card])
  deriving (Haskell.Eq, Haskell.Show)

PlutusTx.unstableMakeIsData ''DealerHand

instance ToJSON DealerHand where
  toJSON (DealerHand (_, cards)) = toJSON cards

instance FromJSON DealerHand where
  parseJSON =
    withArray
      "cards"
      ( Haskell.traverse parseJSON
          >=> (\cs -> Haskell.pure (DealerHand (None, Vector.toList cs)))
      )

reveal :: DealerHand -> [Card]
reveal (DealerHand (Hidden c, cs)) = c : cs
reveal (DealerHand (None, cs)) = cs

data Player
  = Playing {hand :: [Card], bets :: Integer}
  | Standing {hand :: [Card], bets :: Integer}
  | Hitting {hand :: [Card], bets :: Integer}
  deriving stock (Haskell.Show, Haskell.Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''Player

standing :: Player -> Player
standing Playing{hand, bets} = Standing{hand, bets}
standing p = p
{-# INLINEABLE standing #-}

hitting :: Player -> Player
hitting Playing{hand, bets} = Hitting{hand, bets}
hitting p = p
{-# INLINEABLE hitting #-}

isStanding :: Player -> Bool
isStanding Playing{} = False
isStanding Standing{} = True
isStanding Hitting{} = False
{-# INLINEABLE isStanding #-}

isHitting :: Player -> Bool
isHitting Playing{} = False
isHitting Standing{} = False
isHitting Hitting{} = True
{-# INLINEABLE isHitting #-}

nextPlayer :: PlayerId -> Map PlayerId Player -> PlayerId
nextPlayer player players =
  case player of
    PlayerId p
      | p < length players -> PlayerId $ succ p
      | otherwise -> Dealer
    Dealer -> PlayerId 1
{-# INLINEABLE nextPlayer #-}

playerIds :: Integer -> Integer -> [PlayerId]
playerIds lb ub = PlayerId <$> go lb
 where
  go lb' =
    if lb' > ub
      then []
      else lb' : go (succ lb')
{-# INLINEABLE playerIds #-}

newtype RGen = RGen Integer
  deriving newtype (Haskell.Eq, Haskell.Show, ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''RGen

instance ToJSON a => ToJSON (Map PlayerId a) where
  toJSON = toJSON . toList

instance FromJSON a => FromJSON (Map PlayerId a) where
  parseJSON v = parseJSON v <&> Map.fromList

data BlackJack
  = Setup
      { numPlayers :: Integer
      , initialBets :: Map PlayerId Integer
      }
  | BlackJack
      { numPlayers :: Integer
      , next :: PlayerId
      , dealerHand :: DealerHand
      , players :: Map PlayerId Player
      }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''BlackJack

playerHand :: PlayerId -> BlackJack -> [Card]
playerHand _ Setup{} = []
playerHand p BlackJack{players} = maybe [] hand $ Map.lookup p players
{-# INLINEABLE playerHand #-}

type Payoffs = Map PlayerId Integer

data Outcome
  = GameEnds [Card] Payoffs
  | GameContinue BlackJack
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

isEndGame :: Outcome -> Bool
isEndGame GameEnds{} = True
isEndGame GameContinue{} = False

-- | List possible actions (`Play`) in a given state.
possibleActions :: BlackJack -> [Play]
possibleActions Setup{numPlayers, initialBets} =
  let players = playerIds 1 numPlayers
      missingBet p acts = maybe (Bet p : acts) (const acts) $ Map.lookup p initialBets
   in foldr missingBet [] players
possibleActions game@BlackJack{next} = possibleActionsFor next game
{-# INLINEABLE possibleActions #-}

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
      _ -> []
    Dealer -> dealerActions players dealerHand
{-# INLINEABLE possibleActionsFor #-}

-- | This is needed to make `minimum` total on possibly empty lists.
sillyMaximumHandsValue :: Integer
sillyMaximumHandsValue = 50
{-# INLINEABLE sillyMaximumHandsValue #-}

dealerActions :: Map.Map PlayerId Player -> DealerHand -> [Play]
dealerActions players dealerHand =
  case find (isHitting . snd) $ Map.toList players of
    Just (pid, _) -> [DealCard pid]
    Nothing ->
      let value = minimum sillyMaximumHandsValue $ dealerValues dealerHand
       in if value < 17 then [Hit Dealer] else [Stand Dealer]
{-# INLINEABLE dealerActions #-}

dealerValues :: DealerHand -> [Integer]
dealerValues (DealerHand (Hidden c, cards)) = handValues (c : cards)
dealerValues (DealerHand (None, _)) = []
{-# INLINEABLE dealerValues #-}

actionsFor :: PlayerId -> [Card] -> [Play]
actionsFor player hand =
  if minimum 22 (handValues hand) >= 21
    then [Stand player]
    else [Hit player, Stand player]
{-# INLINEABLE actionsFor #-}

minimum :: (Ord a) => a -> [a] -> a
minimum = foldr isMin
 where
  isMin a b
    | a < b = a
    | otherwise = b
{-# INLINEABLE minimum #-}
