module Chess.GameSpec where

import Chess.Game

import Data.Function ((&))
import Test.Hspec (Spec, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (
  Gen,
  Property,
  counterexample,
  elements,
  forAll,
  property, choose, (===),
 )

spec :: Spec
spec = parallel $ do
  prop "can move a pawn one square at start of game" prop_can_move_pawn_one_square
  prop "cannot move a pawn more than 2 squares at start of game" prop_cannot_move_a_pawn_more_than_2_squares

prop_can_move_pawn_one_square :: Property
prop_can_move_pawn_one_square =
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
   forAll (choose (3,6)) $ \ offset ->
    let move = Move (Pos row col) (Pos (row + offset) col)
        result = apply move initialGame
     in case result of
          Right game' ->
            property False
              & counterexample ("expected error, got game: " <> show game')
          Left err -> err === IllegalMove move
              & counterexample ("game: " <> show err)

anyPawn :: Side -> Game -> Gen Position
anyPawn _ _ =
  elements [Pos 1 c | c <- [0 .. 7]]
