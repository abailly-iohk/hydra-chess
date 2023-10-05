module Chess.GameSpec where

import Test.Hspec (Spec, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (
  Property,
  property,
 )

spec :: Spec
spec = parallel $ do
  prop "can move a pawn one square at start of game" prop_can_move_pawn_one_square

prop_can_move_pawn_one_square :: Property
prop_can_move_pawn_one_square = property False
