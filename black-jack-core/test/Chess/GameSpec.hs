{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Chess.GameSpec where

import Chess.Game

import Chess.Generators (anyColumn, anyPawn, anyPos, anyRow, anyValidPawn, generateMove)
import Data.Function ((&))
import Data.Functor ((<&>))
import Test.Hspec (Spec, describe, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (
  Arbitrary (..),
  Property,
  Testable,
  choose,
  conjoin,
  counterexample,
  elements,
  forAll,
  property,
  suchThat,
  tabulate,
  (===),
 )

spec :: Spec
spec = parallel $ do
  describe "Generators" $ do
    prop "generates 2 moves at start for pawns" prop_generate_2_starting_moves_for_pawns
  describe "Pawn" $ do
    prop "can move a white pawn one or 2 squares at start of game" prop_can_move_pawn_one_or_2_squares_at_start
    prop "can move a white pawn one square after start of game" prop_can_move_pawn_one_square_after_start
    prop "cannot move a white pawn more than 2 squares at start of game" prop_cannot_move_a_pawn_more_than_2_squares
    prop "cannot move a white pawn more than 1 square after it moved" prop_cannot_move_a_pawn_more_than_1_square_after_it_moved
    prop "cannot move a white pawn if there's another piece at destination" prop_cannot_move_a_pawn_where_there_is_a_piece
    prop "white pawn can take piece when moving diagonally" prop_pawn_takes_piece_diagonally
    prop "white pawn cannot move diagonally" prop_pawn_cannot_move_diagonally
    prop "white pawn cannot move backwards" prop_pawn_cannot_move_backwards
    prop "can move a black pawn one or 2 squares at start of game" prop_can_move_black_pawn_one_or_2_squares_at_start
    prop "can move a pawn in its column only" prop_can_move_black_pawn_in_its_column_only
  describe "Rook" $ do
    prop "can move horizontally or vertically any number of squares" prop_can_move_rook_horizontally
    prop "can move vertically any number of squares" prop_can_move_rook_vertically
    prop "can take enemy piece at moved location" prop_can_take_enemy_piece
    prop "cannot take enemy piece if move is illegal" prop_cannot_take_enemy_piece_moving_illegally
  describe "Side" $ do
    prop "cannot play same side twice in a row" prop_cannot_play_same_side_twice_in_a_row
  describe "General" $ do
    prop "cannot pass (move to the same position)" prop_cannot_pass

prop_cannot_take_enemy_piece_moving_illegally :: Property
prop_cannot_take_enemy_piece_moving_illegally =
  forAll anyPos $ \pos ->
    forAll arbitrary $ \side ->
      forAll (illegalMoves pos) $ \move@(Move _ to) ->
        let game =
              Game
                side
                [ PieceOnBoard Rook side pos
                , PieceOnBoard Pawn (flipSide side) to
                ]
         in isIllegal game move
 where
  illegalMoves from@(Pos r c) = Move from <$> anyPos `suchThat` \(Pos r' c') -> r' /= r && c' /= c

prop_can_take_enemy_piece :: Property
prop_can_take_enemy_piece =
  forAll anyPos $ \pos ->
    forAll arbitrary $ \side ->
      let startGame = Game side [PieceOnBoard Rook side pos]
       in forAll (elements $ possibleMoves pos startGame) $ \move@(Move _ to) ->
            let game =
                  Game
                    side
                    [ PieceOnBoard Rook side pos
                    , PieceOnBoard Pawn (flipSide side) to
                    ]
             in isLegalMove move game (\game' -> length (findPieces Pawn (flipSide side) game') == 0)

prop_cannot_pass :: Property
prop_cannot_pass =
  forAll arbitrary $ \game@Game{curSide, pieces} ->
    let moveInPlace = filter ((/= curSide) . side) pieces <&> \(PieceOnBoard _ _ pos) -> Move pos pos
     in conjoin (isIllegal game <$> moveInPlace)
          & tabulate "Piece" (show . piece <$> pieces)

prop_can_move_rook_horizontally :: Property
prop_can_move_rook_horizontally =
  forAll anyPos $ \pos@(Pos r c) ->
    forAll arbitrary $ \side ->
      forAll (anyColumn `suchThat` (/= c)) $ \col ->
        let newPos = (Pos r col)
            game = Game side [PieceOnBoard Rook side pos]
            move = Move pos newPos
         in isLegalMove
              move
              game
              (== Game (flipSide side) [PieceOnBoard Rook side newPos])

prop_can_move_rook_vertically :: Property
prop_can_move_rook_vertically =
  forAll anyPos $ \pos@(Pos r c) ->
    forAll arbitrary $ \side ->
      forAll (anyRow `suchThat` (/= r)) $ \row ->
        let newPos = (Pos row c)
            game = Game side [PieceOnBoard Rook side pos]
            move = Move pos newPos
         in isLegalMove
              move
              game
              (== Game (flipSide side) [PieceOnBoard Rook side newPos])

prop_pawn_cannot_move_backwards :: Side -> Property
prop_pawn_cannot_move_backwards side =
  forAll anyPos $ \pos@(Pos r c) ->
    let game = Game side [PieceOnBoard Pawn side pos]
        offset = case side of
          White -> -1
          Black -> 1
        move = Move pos (Pos (r + offset) c)
     in isIllegal game move

prop_generate_2_starting_moves_for_pawns :: Side -> Property
prop_generate_2_starting_moves_for_pawns curSide =
  let game = initialGame{curSide}
   in forAll (anyPawn curSide game) $ \pos ->
        let moves = possibleMoves pos game
         in length moves == 2
              & counterexample ("possible moves: " <> show moves)

prop_can_move_black_pawn_in_its_column_only :: Side -> Property
prop_can_move_black_pawn_in_its_column_only side =
  forAll (anyPawn side initialGame) $ \from@(Pos row col) ->
    forAll
      ( elements [0 .. 7]
          `suchThat` \c' -> c' >= col + 2 || c' <= col - 2
      )
      $ \col' ->
        let offset = case side of
              White -> 1
              Black -> -1
            move = Move from (Pos (row + offset) col')
         in isIllegal initialGame move

prop_cannot_play_same_side_twice_in_a_row :: Side -> Property
prop_cannot_play_same_side_twice_in_a_row side =
  forAll (anyPawn side initialGame) $ \pos ->
    let game = initialGame{curSide = side}
     in forAll (generateMove pos game) $ \move@(Move _ to@(Pos c r)) ->
          let game' = case apply move game of
                Right g -> g
                Left err -> error $ "unexpected invalid move " <> show err
              bit = case side of
                White -> 1
                Black -> -1
              move' = Move to (Pos c (r + bit))
           in isIllegal game' move'

prop_can_move_pawn_one_square_after_start :: Side -> Property
prop_can_move_pawn_one_square_after_start side =
  forAll (anyPos `suchThat` pawnHasMoved side) $ \pos@(Pos row col) ->
    let offset = case side of
          White -> 1
          Black -> -1
        game = Game side [PieceOnBoard Pawn side pos]
        move = Move (Pos row col) (Pos (row + offset) col)
     in isLegalMove move game (/= initialGame)

isLegalMove ::
  (Testable a) =>
  Move ->
  Game ->
  (Game -> a) ->
  Property
isLegalMove move game predicate =
  case apply move game of
    Right game' ->
      predicate game'
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
            let game =
                  Game
                    White
                    [ PieceOnBoard Pawn White pos
                    , PieceOnBoard Pawn Black targetPos
                    , PieceOnBoard Pawn Black otherPos
                    ]
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

prop_pawn_cannot_move_diagonally :: Property
prop_pawn_cannot_move_diagonally =
  forAll (anyValidPawn White) $ \pos@(Pos r c) ->
    forAll (elements [-1, 1]) $ \diagonal ->
      let targetPos = Pos (r + 1) (c + diagonal)
       in forAll (anyPos `suchThat` \p -> p /= pos && p /= targetPos) $ \otherPos ->
            let game =
                  Game
                    White
                    [ PieceOnBoard Pawn White pos
                    , PieceOnBoard Pawn Black otherPos
                    ]
                move = Move pos targetPos
             in isIllegal game move

prop_cannot_move_a_pawn_where_there_is_a_piece :: Property
prop_cannot_move_a_pawn_where_there_is_a_piece =
  forAll (anyPawn White initialGame) $ \(Pos row col) ->
    forAll (choose (1, 2)) $ \offset ->
      let game = Game White [PieceOnBoard Pawn White $ Pos (row + 1) col]
          move = Move (Pos row col) (Pos (row + offset) col)
       in isIllegal game move

prop_can_move_pawn_one_or_2_squares_at_start :: Property
prop_can_move_pawn_one_or_2_squares_at_start =
  forAll (anyPawn White initialGame) $ \(Pos row col) ->
    forAll (choose (1, 2)) $ \offset ->
      let result = apply (Move (Pos row col) (Pos (row + offset) col)) initialGame
       in case result of
            Right game' ->
              game' /= initialGame && length (findPieces Pawn White game') == 8
                & counterexample ("game: " <> show game')
            Left err ->
              property False
                & counterexample ("error: " <> show err)

prop_can_move_black_pawn_one_or_2_squares_at_start :: Property
prop_can_move_black_pawn_one_or_2_squares_at_start =
  forAll (anyPawn Black game) $ \(Pos row col) ->
    forAll (choose (1, 2)) $ \offset ->
      let move = Move (Pos row col) (Pos (row - offset) col)
          result = apply move game
       in case result of
            Right game' ->
              game' /= game && length (findPieces Pawn Black game') == 8
                & counterexample ("end game: " <> show game')
                & counterexample ("move: " <> show move)
            Left err ->
              property False
                & counterexample ("error: " <> show err)
 where
  game = initialGame{curSide = Black}

prop_cannot_move_a_pawn_more_than_2_squares :: Side -> Property
prop_cannot_move_a_pawn_more_than_2_squares side =
  forAll (anyPawn side initialGame) $ \(Pos row col) ->
    forAll (choose (3, 6)) $ \offset ->
      let bit = case side of
            White -> 1
            Black -> -1
          move = Move (Pos row col) (Pos (row + offset * bit) col)
       in isIllegal initialGame move

prop_cannot_move_a_pawn_more_than_1_square_after_it_moved :: Side -> Property
prop_cannot_move_a_pawn_more_than_1_square_after_it_moved side =
  forAll (anyPos `suchThat` pawnHasMoved side) $ \pos@(Pos row col) ->
    let game = Game side [PieceOnBoard Pawn side pos]
        move = Move (Pos row col) (Pos (row + 2) col)
     in isIllegal game move

-- * Generic Properties

isIllegal :: Game -> Move -> Property
isIllegal game move =
  case apply move game of
    Right game' ->
      property False
        & counterexample ("game': " <> show game')
        & counterexample ("move: " <> show move)
    Left err ->
      err
        === IllegalMove move
        & counterexample ("game: " <> show err)

pawnHasMoved :: Side -> Position -> Bool
pawnHasMoved side (Pos r _) = case side of
  White -> r > 1
  Black -> r < 6
