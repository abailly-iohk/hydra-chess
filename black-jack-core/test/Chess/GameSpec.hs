module Chess.GameSpec where

import Chess.Game

import Data.Function ((&))
import Test.Hspec (Spec, describe, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (
  Gen,
  Property,
  choose,
  counterexample,
  elements,
  forAll,
  property,
  (===),
 )

spec :: Spec
spec = parallel $ do
  describe "Pawn" $ do
    prop "can move a pawn one or 2 squares at start of game" prop_can_move_pawn_one_or_2_squares_at_start
    prop "cannot move a pawn more than 2 squares at start of game" prop_cannot_move_a_pawn_more_than_2_squares
    prop "cannot move a pawn more than 1 square after it moved" prop_cannot_move_a_pawn_more_than_1_square_after_it_moved

prop_can_move_pawn_one_or_2_squares_at_start :: Property
prop_can_move_pawn_one_or_2_squares_at_start =
  forAll (anyPawn White initialGame) $ \(Pos row col) ->
    let result = apply (Move (Pos row col) (Pos (row + 1) col)) initialGame
     in case result of
          Right game' ->
            game' /= initialGame
              & counterexample ("game: " <> show game')
          Left err ->
            property False
              & counterexample ("error: " <> show err)

prop_cannot_move_a_pawn_more_than_2_squares :: Property
prop_cannot_move_a_pawn_more_than_2_squares =
  forAll (anyPawn White initialGame) $ \(Pos row col) ->
    forAll (choose (3, 6)) $ \offset ->
      let move = Move (Pos row col) (Pos (row + offset) col)
          result = apply move initialGame
       in case result of
            Right game' ->
              property False
                & counterexample ("expected error, got game: " <> show game')
            Left err ->
              err
                === IllegalMove move
                & counterexample ("game: " <> show err)

prop_cannot_move_a_pawn_more_than_1_square_after_it_moved :: Property
prop_cannot_move_a_pawn_more_than_1_square_after_it_moved =
  forAll (anyPawn White initialGame) $ \(Pos row col) ->
    forAll (choose (1, 2)) $ \offset ->
      let move = Move (Pos (row + offset) col) (Pos (row + offset + 2) col)
          result =
            apply (Move (Pos row col) (Pos (row + offset) col)) initialGame
              >>= apply move
       in case result of
            Right game' ->
              property False
                & counterexample ("game: " <> show game')
                & counterexample ("move: " <> show move)
            Left err ->
              err
                === IllegalMove move
                & counterexample ("game: " <> show err)

anyPawn :: Side -> Game -> Gen Position
anyPawn _ _ =
  elements [Pos 1 c | c <- [0 .. 7]]
