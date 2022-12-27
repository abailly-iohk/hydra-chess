{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module BlackJack.GameSpec where

import BlackJack.Game

import Control.Arrow (second)
import Data.Data (Proxy (Proxy))
import Data.Function ((&))
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.TypeLits (KnownNat, Nat, natVal)
import Test.Hspec (Spec, describe, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (
  Arbitrary (..),
  Gen,
  Positive (getPositive),
  Property,
  checkCoverage,
  choose,
  counterexample,
  coverTable,
  discard,
  elements,
  forAll,
  getSmall,
  suchThat,
  tabulate,
  vectorOf,
  (===),
  (==>),
 )

spec :: Spec
spec = parallel $ do
  prop "deal cards when all players have placed their bets" prop_deal_cards_after_placing_bet
  describe "player" $ do
    prop "can only stand when cards total >= 21" prop_stand_when_card_total_at_21
    prop "can hit when cards total < 21" prop_hit_when_card_total_lower_than_21
    prop "has possible actions when setting up" prop_possible_actions_for_all_players_when_setting_up
    prop "has possible actions when playing" prop_possible_actions_for_one_player_when_playing
    prop "advance to next player or dealer when standing" prop_advance_next_player_when_standing
    prop "don't advance to next player or dealer when hitting" prop_dont_advance_next_player_when_hitting
    prop "is dealt a new card when hits" prop_deals_new_card_when_hit
  describe "dealer" $ do
    prop "plays after last player" prop_dealer_plays_after_last_player
    prop "keeps playing when hitting " prop_dealer_keeps_playing_when_hitting
    prop "dealer hits at 16 and stand at 17" prop_dealer_hits_at_16_stand_at_17
    prop "game ends after dealer stands" prop_game_ends_after_dealer_stands
  prop "hand values ignores duplicates" prop_hand_values_ignore_duplicates
  prop "end game when everyone stands" prop_end_game_when_everyone_stands
  describe "payoffs" $ do
    prop "winning player doubles bets when hand is over dealer's" prop_winning_doubles_bet
    prop "winning player gets 3/2 bet when hand is a blackjack" prop_winning_blackjack_more_than_doubles_bet
    prop "player gets back bet when tied" prop_tie_yields_bet
    describe "player lose their bets" $ do
      prop "when hand is over 21" prop_hand_over_21_loses_bet
      prop "when hand is over 21 and dealer over 21" prop_hand_over_21_loses_even_if_dealer_over_21
      prop "when hand is under dealer's " prop_hand_under_dealers_loses_bet

genInitialSetup :: Gen BlackJack
genInitialSetup =
  newGame <$> (getSmall . getPositive <$> arbitrary) <*> arbitrary

prop_deal_cards_after_placing_bet :: Property
prop_deal_cards_after_placing_bet =
  forAll genInitialSetup $ \game ->
    let ids = playerIds 1 (numPlayers game)
        plays = [Bet p | p <- ids]
        GameContinue game' = runPlays plays game
     in all (\p -> length (playerHand p game') == 2) ids
          & counterexample ("game' : " <> show game')

prop_stand_when_card_total_at_21 :: Property
prop_stand_when_card_total_at_21 =
  forAll (genHandOver 21) $ \cards ->
    let game = mkGame 1 1 42 [(1, (100, cards))]
     in possibleActionsFor 1 game == [Stand 1]
          & counterexample ("game: " <> show game)

gen2Cards :: Gen [Card]
gen2Cards = do
  c <- arbitrary
  c' <- arbitrary
  pure [c, c']

prop_hit_when_card_total_lower_than_21 :: Property
prop_hit_when_card_total_lower_than_21 =
  forAll (genHandUnder 21) $ \cards ->
    let game = mkGame 1 1 42 [(1, (100, cards))]
        actions = possibleActionsFor 1 game
     in Hit 1 `elem` actions
          & counterexample ("game: " <> show game)
          & counterexample ("actions: " <> show actions)

prop_possible_actions_for_all_players_when_setting_up :: Property
prop_possible_actions_for_all_players_when_setting_up =
  forAll genInitialSetup $ \game ->
    let actions = possibleActions game
     in Set.fromList (Bet <$> playerIds 1 (numPlayers game)) === Set.fromList actions
          & counterexample ("Actions: " <> show actions)

newtype RunningGame (n :: Nat) = RunningGame BlackJack
  deriving newtype (Eq, Show)

instance forall n. KnownNat n => Arbitrary (RunningGame n) where
  arbitrary = do
    let minPlayers = fromIntegral $ natVal (Proxy @n)
    numPlayers <- (getSmall . getPositive <$> arbitrary) `suchThat` \n -> n >= minPlayers
    hands <- vectorOf numPlayers (genHandUnder 21)
    next <- PlayerId <$> choose (1, numPlayers)
    seed <- arbitrary
    pure $ RunningGame $ mkGame numPlayers next seed $ second (100,) <$> zip (playerIds 1 numPlayers) hands

  shrink (RunningGame game@BlackJack{players})
    | length players > fromIntegral (natVal (Proxy @n)) =
      [RunningGame game{players = Map.delete (PlayerId $ length players) players}]
  shrink _ = []

prop_possible_actions_for_one_player_when_playing :: RunningGame 1 -> Property
prop_possible_actions_for_one_player_when_playing (RunningGame game) =
  let actions = possibleActions game
   in Set.fromList (forPlayer <$> actions) === Set.singleton (next game)
        & counterexample ("Actions: " <> show actions)

genPlay :: BlackJack -> Gen Play
genPlay = elements . possibleActions

prop_advance_next_player_when_standing :: RunningGame 2 -> Property
prop_advance_next_player_when_standing (RunningGame game) =
  let GameContinue game' = play game (Stand $ next game)
   in next game `notElem` (forPlayer <$> possibleActions game')
        & counterexample ("updated game: " <> show game')

prop_dont_advance_next_player_when_hitting :: RunningGame 2 -> Property
prop_dont_advance_next_player_when_hitting (RunningGame game) =
  let GameContinue game' = play game (Hit $ next game)
   in next game `elem` (forPlayer <$> possibleActions game')
        & counterexample ("updated game: " <> show game')

prop_dealer_plays_after_last_player :: RunningGame 2 -> Property
prop_dealer_plays_after_last_player (RunningGame game) =
  let next = PlayerId (length $ players game)
      GameContinue game' = play game{next} (Stand next)
   in all (== Dealer) (forPlayer <$> possibleActions game')
        & counterexample ("Last player: " <> show next)
        & counterexample ("After player stands: " <> show game')

prop_dealer_keeps_playing_when_hitting :: RunningGame 1 -> Property
prop_dealer_keeps_playing_when_hitting (RunningGame game) =
  let game' = game{next = Dealer}
      GameContinue game'' = play game' (Hit Dealer)
   in next game'' == Dealer
        & counterexample ("After dealer hits: " <> show game'')

prop_game_ends_after_dealer_stands :: RunningGame 1 -> Property
prop_game_ends_after_dealer_stands (RunningGame game) =
  let game' = game{next = Dealer}
      outcome = play game' (Stand Dealer)
   in isEndGame outcome
        & counterexample ("outcome: " <> show outcome)

prop_dealer_hits_at_16_stand_at_17 :: DealerHand -> Property
prop_dealer_hits_at_16_stand_at_17 hand =
  let acts = dealerActions hand
   in acts == [Hit Dealer] || acts == [Stand Dealer]
        & counterexample ("Actions: " <> show acts)
        & tabulate "Actions" (concat $ take 1 . words . show <$> acts)
        & coverTable "Actions" [("Hit", 30), ("Stand", 70)]
        & checkCoverage

prop_deals_new_card_when_hit :: RunningGame 2 -> Property
prop_deals_new_card_when_hit (RunningGame game) =
  forAll (genPlay game) $ \p ->
    let GameContinue game' = play game p
        cardsBefore = playerHand (forPlayer p) game
        cardsAfter = playerHand (forPlayer p) game'
     in isHit p ==> length cardsAfter == length cardsBefore + 1
          & counterexample ("Before: " <> show cardsBefore)
          & counterexample ("After: " <> show cardsAfter)

prop_hand_values_ignore_duplicates :: [Card] -> Property
prop_hand_values_ignore_duplicates hand =
  let values = handValues hand
   in Set.toList (Set.fromList values) == values
        & counterexample ("Values: " <> show values)

prop_end_game_when_everyone_stands :: RunningGame 2 -> Property
prop_end_game_when_everyone_stands (RunningGame game) =
  let game' = allStand game
   in any isEndGame game'
        & counterexample ("Game: " <> show (head . words . show <$> game'))

allStand :: BlackJack -> [Outcome]
allStand Setup{} = error "Unexpected setup"
allStand game@BlackJack{next} =
  case play game (Stand next) of
    ends@GameEnds{} -> [ends]
    cont@(GameContinue game') -> cont : allStand game'

genHandOver :: Int -> Gen [Card]
genHandOver lb = arbitrary >>= go . (: [])
 where
  go cs =
    let mn = minimum $ handValues cs
     in if mn > lb
          then pure cs
          else arbitrary >>= go . (: cs)

genWinningHand :: Int -> Gen [Card]
genWinningHand lb = arbitrary >>= go . (: [])
 where
  go cs =
    let mn = minimum $ handValues cs
     in if
            | mn > 21 -> discard
            | mn > lb -> pure cs
            | otherwise -> arbitrary >>= go . (: cs)

genHandUnder :: Int -> Gen [Card]
genHandUnder lb = arbitrary >>= go . (: [])
 where
  go cs =
    let mx = maximum $ handValues cs
     in if
            | mx < lb && mx <= 21 -> pure cs
            | mx > 21 -> discard
            | otherwise -> arbitrary >>= go . (: cs)

prop_winning_doubles_bet :: DealerHand -> Property
prop_winning_doubles_bet dealer =
  forAll (genWinningHand (maximum $ dealerValues dealer)) $ \cards ->
    payoff dealer (Playing cards 100) == 2 * 100
      && payoff dealer (Standing cards 100) == 2 * 100
      & tabulate "Score" (show <$> filter (<= 21) (handValues cards))

prop_hand_over_21_loses_bet :: DealerHand -> Property
prop_hand_over_21_loses_bet dealer =
  forAll (genHandOver 21) $ \cards ->
    payoff dealer (Playing cards 100) == 0
      && payoff dealer (Standing cards 100) == 0

prop_hand_over_21_loses_even_if_dealer_over_21 :: Property
prop_hand_over_21_loses_even_if_dealer_over_21 =
  forAll (genHandOver 21) $ \(mkDealerHand -> dealer) ->
    forAll (genHandOver 21) $ \cards ->
      let gain = payoff dealer (Playing cards 100)
       in gain == 0
            && payoff dealer (Standing cards 100) == 0
            & counterexample ("Gain: " <> show gain)

prop_hand_under_dealers_loses_bet :: DealerHand -> Property
prop_hand_under_dealers_loses_bet dealer =
  forAll (genHandUnder (maximum $ dealerValues dealer)) $ \cards ->
    let gain = payoff dealer (Playing cards 100)
     in gain == 0 && payoff dealer (Standing cards 100) == 0
          & counterexample ("Gain: " <> show gain)

genBlackJack :: Gen [Card]
genBlackJack = do
  c <- Card <$> arbitrary <*> elements [Ten, Jack, Queen, King]
  c' <- Card <$> arbitrary <*> pure Ace
  pure [c, c']

prop_winning_blackjack_more_than_doubles_bet :: DealerHand -> Property
prop_winning_blackjack_more_than_doubles_bet dealer =
  forAll genBlackJack $ \cards ->
    let gain = payoff dealer (Playing cards 100)
     in gain == 250
          & counterexample ("Gain: " <> show gain)
          & counterexample ("Value dealer: " <> show (handValues $ reveal dealer))
          & counterexample ("Value player: " <> show (handValues cards))

prop_tie_yields_bet :: DealerHand -> Property
prop_tie_yields_bet dealer =
  let dealerHand = reveal dealer
      gain = payoff dealer (Playing dealerHand 100)
   in gain == 100
        & counterexample ("Hand: " <> show dealerHand)
        & counterexample ("Gain: " <> show gain)
        & counterexample ("Values: " <> show (handValues dealerHand))
