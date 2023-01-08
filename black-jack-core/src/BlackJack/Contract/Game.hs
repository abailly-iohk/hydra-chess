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
import qualified Data.Foldable as Haskell
import qualified Data.Traversable as Haskell
import GHC.Generics (Generic)
import qualified PlutusTx
import PlutusTx.AssocMap (Map)
import qualified PlutusTx.AssocMap as Map
import System.Random (Uniform)
import qualified Prelude as Haskell

data PlayerId
  = PlayerId {playerId :: Integer}
  | Dealer
  deriving stock (Haskell.Eq, Haskell.Ord, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''PlayerId

instance Eq PlayerId where
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

cardValues :: Card -> [Integer] -> [Integer]
cardValues card [] = cardValue card
cardValues card values = [vals + v | vals <- values, v <- cardValue card]

cardValue :: Card -> [Integer]
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
          >=> (\cs -> Haskell.pure (DealerHand (None, Haskell.toList cs)))
      )

reveal :: DealerHand -> [Card]
reveal (DealerHand (Hidden c, cs)) = c : cs
reveal (DealerHand (None, cs)) = cs

actionsFor :: PlayerId -> [Card] -> [Play]
actionsFor player hand
  | minimum 22 (handValues hand) >= 21 = [Stand player]
  | otherwise = [Hit player, Stand player]

minimum :: (Ord a) => a -> [a] -> a
minimum = foldr isMin
 where
  isMin a b
    | a < b = a
    | otherwise = b

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

hitting :: Player -> Player
hitting Playing{hand, bets} = Hitting{hand, bets}
hitting p = p

isStanding :: Player -> Bool
isStanding Playing{} = False
isStanding Standing{} = True
isStanding Hitting{} = False

isHitting :: Player -> Bool
isHitting Playing{} = False
isHitting Standing{} = False
isHitting Hitting{} = True

nextPlayer :: PlayerId -> Map PlayerId Player -> PlayerId
nextPlayer player players =
  case player of
    PlayerId p
      | p < length players -> PlayerId $ succ p
      | otherwise -> Dealer
    Dealer -> PlayerId 1

playerIds :: Integer -> Integer -> [PlayerId]
playerIds lb ub = PlayerId <$> [lb .. ub]

newtype RGen = RGen Integer
  deriving newtype (Haskell.Eq, Haskell.Show, ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''RGen

instance ToJSON a => ToJSON (Map PlayerId a) where
  toJSON = Haskell.undefined

instance ToJSON a => FromJSON (Map PlayerId a) where
  parseJSON = Haskell.undefined

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

type Payoffs = Map PlayerId Integer

data Outcome
  = GameEnds [Card] Payoffs
  | GameContinue BlackJack
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

isEndGame :: Outcome -> Bool
isEndGame GameEnds{} = True
isEndGame GameContinue{} = False
