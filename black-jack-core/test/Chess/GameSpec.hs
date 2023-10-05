module Chess.GameSpec where

import Chess.Game

import Test.Hspec (Spec, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (
  Property,
  property, forAll, Gen,
 )

spec :: Spec
spec = parallel $ do
  prop "can move a pawn one square at start of game" prop_can_move_pawn_one_square

prop_can_move_pawn_one_square :: Property
prop_can_move_pawn_one_square =
  forAll (anyPawn White initialGame) $ \ (Pos row col) ->
     property $ apply (Move (Pos row col) (Pos (row + 1) col)) initialGame

anyPawn :: Side -> Game -> Gen Position
anyPawn = undefined
