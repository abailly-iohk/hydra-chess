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
  property,
 )

spec :: Spec
spec = parallel $ do
  prop "can move a pawn one square at start of game" prop_can_move_pawn_one_square

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

anyPawn :: Side -> Game -> Gen Position
anyPawn _ _ =
  elements [Pos 1 c | c <- [0 .. 7]]
