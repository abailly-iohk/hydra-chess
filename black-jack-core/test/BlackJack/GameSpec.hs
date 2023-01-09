{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module BlackJack.GameSpec where

import BlackJack.Game

import BlackJack.Contract.Game (dealerActions, forPlayer, isEndGame, possibleActions, possibleActionsFor)
import Control.Arrow (second)
import Control.Monad.State (evalState, runState)
import Data.Data (Proxy (Proxy))
import Data.Function ((&))
import qualified Data.Set as Set
import GHC.TypeLits (KnownNat, Nat, natVal)
import System.Random (StdGen, mkStdGen)
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
    prop "advance to dealer when hitting" prop_advance_to_dealer_when_hitting
    prop "advance to player after dealing card" prop_advance_to_player_after_dealing_card
    prop "is dealt a new card by dealer when hits" prop_deals_new_card_when_hitting
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
  newGame <$> (getSmall . getPositive <$> arbitrary)

instance Arbitrary StdGen where
  arbitrary = mkStdGen <$> arbitrary

prop_deal_cards_after_placing_bet :: StdGen -> Property
prop_deal_cards_after_placing_bet seed =
  forAll genInitialSetup $ \game ->
    let ids = playerIds 1 (numPlayers game)
        plays = [Bet p | p <- ids]
        GameContinue game' = evalState (runPlays plays game) seed
     in all (\p -> length (playerHand p game') == 2) ids
          & counterexample ("game' : " <> show game')

prop_stand_when_card_total_at_21 :: StdGen -> Property
prop_stand_when_card_total_at_21 seed =
  forAll (genHandOver 21) $ \cards ->
    let game = evalState (mkGame 1 1 [(1, (100, cards))]) seed
        actions = possibleActionsFor 1 game
     in actions == [Stand 1]
          & counterexample ("game: " <> show game)
          & counterexample ("actions: " <> show actions)

gen2Cards :: Gen [Card]
gen2Cards = do
  c <- arbitrary
  c' <- arbitrary
  pure [c, c']

prop_hit_when_card_total_lower_than_21 :: StdGen -> Property
prop_hit_when_card_total_lower_than_21 seed =
  forAll (genHandUnder 21) $ \cards ->
    let game = evalState (mkGame 1 1 [(1, (100, cards))]) seed
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

data RunningGame (n :: Nat) = RunningGame BlackJack StdGen
  deriving stock (Eq, Show)

instance forall n. KnownNat n => Arbitrary (RunningGame n) where
  arbitrary = do
    let minPlayers = fromIntegral $ natVal (Proxy @n)
    numPlayers :: Integer <- (getSmall . getPositive <$> arbitrary) `suchThat` \n -> n >= minPlayers
    hands <- vectorOf (fromInteger numPlayers) (genHandUnder 21)
    next <- PlayerId <$> choose (1, numPlayers)
    seed <- arbitrary
    let (game', seed') = runState (mkGame numPlayers next $ second (100,) <$> zip (playerIds 1 numPlayers) hands) seed
    pure $ RunningGame game' seed'

  shrink (RunningGame game@BlackJack{players} seed)
    | mapLength players > fromIntegral (natVal (Proxy @n)) =
      [RunningGame game{players = deletePlayer (PlayerId $ fromIntegral $ mapLength players) players} seed]
  shrink _ = []

prop_possible_actions_for_one_player_when_playing :: RunningGame 1 -> Property
prop_possible_actions_for_one_player_when_playing (RunningGame game _) =
  let actions = possibleActions game
   in Set.fromList (forPlayer <$> actions) === Set.singleton (next game)
        & counterexample ("Actions: " <> show actions)

genPlay :: BlackJack -> Gen Play
genPlay = elements . possibleActions

prop_advance_next_player_when_standing :: RunningGame 2 -> Property
prop_advance_next_player_when_standing (RunningGame game seed) =
  let GameContinue game' = evalState (play game (Stand $ next game)) seed
   in next game `notElem` (forPlayer <$> possibleActions game')
        & counterexample ("updated game: " <> show game')

prop_advance_to_dealer_when_hitting :: RunningGame 2 -> Property
prop_advance_to_dealer_when_hitting (RunningGame game seed) =
  let GameContinue game' = evalState (play game (Hit $ next game)) seed
      actions = possibleActions game'
   in next game' == Dealer && actions == [DealCard $ next game]
        & counterexample ("updated game: " <> show game')
        & counterexample ("actions: " <> show actions)

prop_advance_to_player_after_dealing_card :: RunningGame 2 -> Property
prop_advance_to_player_after_dealing_card (RunningGame game seed) =
  forAll (onePlayerHitting game) $ \(pid, game') ->
    let GameContinue game'' = evalState (play game' (DealCard pid)) seed
        actions = possibleActions game''
     in next game'' == pid
          & counterexample ("updated game: " <> show game'')
          & counterexample ("actions: " <> show actions)

prop_dealer_plays_after_last_player :: RunningGame 2 -> Property
prop_dealer_plays_after_last_player (RunningGame game seed) =
  let next = PlayerId (fromIntegral $ mapLength $ players game)
      GameContinue game' = evalState (play game{next} (Stand next)) seed
   in all (== Dealer) (forPlayer <$> possibleActions game')
        & counterexample ("Last player: " <> show next)
        & counterexample ("After player stands: " <> show game')

prop_dealer_keeps_playing_when_hitting :: RunningGame 1 -> Property
prop_dealer_keeps_playing_when_hitting (RunningGame game seed) =
  let game' = game{next = Dealer}
      GameContinue game'' = evalState (play game' (Hit Dealer)) seed
   in next game'' == Dealer
        & counterexample ("After dealer hits: " <> show game'')

prop_game_ends_after_dealer_stands :: RunningGame 1 -> Property
prop_game_ends_after_dealer_stands (RunningGame game seed) =
  let game' = game{next = Dealer}
      outcome = evalState (play game' (Stand Dealer)) seed
   in isEndGame outcome
        & counterexample ("outcome: " <> show outcome)

prop_dealer_hits_at_16_stand_at_17 :: RunningGame 1 -> DealerHand -> Property
prop_dealer_hits_at_16_stand_at_17 (RunningGame game _) hand =
  let acts = dealerActions (players game) hand
   in acts == [Hit Dealer] || acts == [Stand Dealer]
        & counterexample ("Actions: " <> show acts)
        & tabulate "Actions" (concat $ take 1 . words . show <$> acts)
        & coverTable "Actions" [("Hit", 30), ("Stand", 70)]
        & checkCoverage

prop_deals_new_card_when_hitting :: RunningGame 2 -> Property
prop_deals_new_card_when_hitting (RunningGame game seed) =
  forAll (onePlayerHitting game) $ \(p, game') ->
    let GameContinue game'' = evalState (play game' $ head $ possibleActions game') seed
        cardsBefore = playerHand p game'
        cardsAfter = playerHand p game''
     in length cardsAfter == length cardsBefore + 1
          & counterexample ("Before: " <> show cardsBefore)
          & counterexample ("After: " <> show cardsAfter)

onePlayerHitting :: BlackJack -> Gen (PlayerId, BlackJack)
onePlayerHitting game@BlackJack{numPlayers, players} = do
  pid <- PlayerId <$> choose (1, numPlayers)
  pure (pid, game{next = Dealer, players = assocMapUpdate (Just . hitting) pid players})
onePlayerHitting Setup{} = error "not implemented"

prop_hand_values_ignore_duplicates :: [Card] -> Property
prop_hand_values_ignore_duplicates hand =
  let values = handValues hand
   in Set.toList (Set.fromList values) == values
        & counterexample ("Values: " <> show values)

prop_end_game_when_everyone_stands :: RunningGame 2 -> Property
prop_end_game_when_everyone_stands (RunningGame game seed) =
  let game' = allStand game seed
   in any isEndGame game'
        & counterexample ("Game: " <> show (head . words . show <$> game'))

allStand :: BlackJack -> StdGen -> [Outcome]
allStand Setup{} _ = error "Unexpected setup"
allStand game@BlackJack{next} seed =
  case runState (play game (Stand next)) seed of
    (ends@GameEnds{}, _) -> [ends]
    (cont@(GameContinue game'), seed') -> cont : allStand game' seed'

genHandOver :: Integer -> Gen [Card]
genHandOver lb = arbitrary >>= go . (: [])
 where
  go cs =
    let mn = minimum $ handValues cs
     in if mn > lb
          then pure cs
          else arbitrary >>= go . (: cs)

genWinningHand :: Integer -> Gen [Card]
genWinningHand lb = arbitrary >>= go . (: [])
 where
  go cs =
    let mn = minimum $ handValues cs
     in if
            | mn > 21 -> discard
            | mn > lb -> pure cs
            | otherwise -> arbitrary >>= go . (: cs)

genHandUnder :: Integer -> Gen [Card]
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
