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
  suchThat,
  (===),
 )

spec :: Spec
spec = parallel $ do
  describe "Pawn" $ do
    prop "can move a pawn one or 2 squares at start of game" prop_can_move_pawn_one_or_2_squares_at_start
    prop "can move a pawn one square after start of game" prop_can_move_pawn_one_square_after_start
    prop "cannot move a pawn more than 2 squares at start of game" prop_cannot_move_a_pawn_more_than_2_squares
    prop "cannot move a pawn more than 1 square after it moved" prop_cannot_move_a_pawn_more_than_1_square_after_it_moved
    prop "cannot move a pawn if there's another piece at destination" prop_cannot_move_a_pawn_where_there_is_a_piece
    prop "can take piece when moving diagonally" prop_pawn_takes_piece_diagonally

prop_can_move_pawn_one_square_after_start :: Property
prop_can_move_pawn_one_square_after_start =
  forAll (anyPawn White initialGame) $ \(Pos row col) ->
    let move = Move (Pos (row + 1) col) (Pos (row + 2) col)
        result =
          apply (Move (Pos row col) (Pos (row + 1) col)) initialGame
            >>= apply move
     in case result of
          Right game' ->
            game' /= initialGame && length (findPieces Pawn White game') == 8
              & counterexample ("game: " <> show game')
          Left err ->
            property False
              & counterexample ("error: " <> show err)

prop_pawn_takes_piece_diagonally :: Property
prop_pawn_takes_piece_diagonally =
  forAll (anyValidPawn White) $ \pos@(Pos r c) ->
    forAll (elements [-1, 1]) $ \diagonal ->
      let targetPos = Pos (r + 1) (c + diagonal)
       in forAll (anyPos `suchThat` \p -> p /= pos && p /= targetPos) $ \otherPos ->
            let game = Game [(Pawn, White, pos), (Pawn, Black, targetPos), (Pawn, Black, otherPos)]
                move = Move pos targetPos
             in case apply move game of
                  Right game' ->
                    length (findPieces Pawn Black game')
                      === 1
                      & counterexample ("end game: " <> show game')
                      & counterexample ("move: " <> show move)
                      & counterexample ("start game: " <> show game)
                  Left err ->
                    property False
                      & counterexample ("game: " <> show err)

anyValidPawn :: Side -> Gen Position
anyValidPawn _ =
  elements [Pos r c | r <- [1 .. 6], c <- [0 .. 7]]

prop_cannot_move_a_pawn_where_there_is_a_piece :: Property
prop_cannot_move_a_pawn_where_there_is_a_piece =
  forAll (anyPawn White initialGame) $ \(Pos row col) ->
    forAll (choose (1, 2)) $ \offset ->
      let game = Game [(Pawn, White, Pos (row + 1) col)]
          move = Move (Pos row col) (Pos (row + offset) col)
          result = apply move game
       in case result of
            Right game' ->
              property False
                & counterexample ("game: " <> show game')
                & counterexample ("move: " <> show move)
            Left err ->
              err
                === IllegalMove move
                & counterexample ("game: " <> show err)

prop_can_move_pawn_one_or_2_squares_at_start :: Property
prop_can_move_pawn_one_or_2_squares_at_start =
  forAll (anyPawn White initialGame) $ \(Pos row col) ->
    let result = apply (Move (Pos row col) (Pos (row + 1) col)) initialGame
     in case result of
          Right game' ->
            game' /= initialGame && length (findPieces Pawn White game') == 8
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
anyPawn side game =
  elements $ ((\(_, _, pos) -> pos) <$> findPieces Pawn side game)

anyPos :: Gen Position
anyPos =
  elements [Pos r c | r <- [0 .. 7], c <- [0 .. 7]]
