{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialize -fdefer-type-errors #-}

module Chess.Game where

import PlutusTx.Prelude

import Control.Monad (guard)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified PlutusTx
import qualified Prelude as Haskell

type Row = Integer
type Col = Integer

data Position = Pos Row Col
  deriving (Haskell.Eq, Haskell.Show, Generic, ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''Position

instance Eq Position where
  Pos r c == Pos r' c' = r == r' && c == c'

newtype Path = Path {positions :: [Position]}
  deriving (Haskell.Eq, Haskell.Show)

instance Eq Path where
  Path p == Path p' = p == p'

data Piece = Pawn | Rook | Bishop
  deriving (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''Piece

instance Eq Piece where
  Pawn == Pawn = True
  Rook == Rook = True
  Bishop == Bishop = True
  _ == _ = False

data Side = White | Black
  deriving (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''Side

instance Eq Side where
  White == White = True
  Black == Black = True
  _ == _ = False

flipSide :: Side -> Side
flipSide White = Black
flipSide Black = White

data PieceOnBoard = PieceOnBoard {piece :: Piece, side :: Side, pos :: Position}
  deriving (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''PieceOnBoard

instance Eq PieceOnBoard where
  PieceOnBoard p s pos == PieceOnBoard p' s' pos' = p == p' && s == s' && pos == pos'

data Game = Game
  { curSide :: Side
  , pieces :: [PieceOnBoard]
  }
  deriving (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''Game

instance Eq Game where
  Game s p == Game s' p' = s == s' && p == p'

initialGame :: Game
initialGame =
  Game White $
    [PieceOnBoard Pawn White (Pos 1 c) | c <- [0 .. 7]]
      <> [PieceOnBoard Pawn Black (Pos 6 c) | c <- [0 .. 7]]
      <> [PieceOnBoard Rook Black (Pos 7 0), PieceOnBoard Rook Black (Pos 7 7)]
      <> [PieceOnBoard Rook White (Pos 0 0), PieceOnBoard Rook White (Pos 0 7)]
      <> [PieceOnBoard Bishop Black (Pos 7 2), PieceOnBoard Bishop Black (Pos 7 5)]
      <> [PieceOnBoard Bishop White (Pos 0 2), PieceOnBoard Bishop White (Pos 0 5)]

findPieces :: Piece -> Side -> Game -> [PieceOnBoard]
findPieces piece' side' Game{pieces} =
  filter (\PieceOnBoard{piece, side} -> piece == piece' && side == side') pieces

data Move = Move Position Position
  deriving (Haskell.Eq, Haskell.Show, Generic, ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''Move

data IllegalMove = IllegalMove Move
  deriving (Haskell.Eq, Haskell.Show)

PlutusTx.unstableMakeIsData ''IllegalMove

apply :: Move -> Game -> Either IllegalMove Game
apply move@(Move from to) game@(Game curSide _)
  | from == to = Left $ IllegalMove move
  | otherwise =
      case pieceAt from game of
        Just (PieceOnBoard Pawn White _) | curSide == White -> moveWhitePawn move game
        Just (PieceOnBoard Pawn Black _) | curSide == Black -> moveBlackPawn move game
        Just (PieceOnBoard Rook side _) | curSide == side -> moveRook move game
        Just (PieceOnBoard Bishop side _) | curSide == side -> moveBishop move game
        _ -> Left $ IllegalMove move
{-# INLINEABLE apply #-}

moveRook :: Move -> Game -> Either IllegalMove Game
moveRook move@(Move from@(Pos row col) to@(Pos row' col')) game =
  if
      | row' == row || col' == col ->
          case game `firstPieceOn` path from to of
            Just (PieceOnBoard{pos})
              | pos == to -> takePiece game from to
              | otherwise -> Left $ IllegalMove move
            Nothing -> Right $ movePiece game from to
      | otherwise ->
          Left $ IllegalMove move
{-# INLINEABLE moveRook #-}

moveBishop :: Move -> Game -> Either IllegalMove Game
moveBishop _move@(Move from@(Pos _row _col) to@(Pos _row' _col')) game =
  Right $ movePiece game from to
{-# INLINEABLE moveBishop #-}

moveWhitePawn :: Move -> Game -> Either IllegalMove Game
moveWhitePawn move@(Move from@(Pos row col) to@(Pos row' col')) game =
  if
      | (row' - row) == 1 && abs (col' - col) == 1 ->
          takePiece game from to
      | isJust (game `firstPieceOn` path from to) ->
          Left $ IllegalMove move
      | row >= 2 && row' - row == 1 && col == col' ->
          Right $ movePiece game from to
      | row == 1 && row' - row <= 2 && row' > row && col == col' ->
          Right $ movePiece game from to
      | otherwise ->
          Left $ IllegalMove move
{-# INLINEABLE moveWhitePawn #-}

moveBlackPawn :: Move -> Game -> Either IllegalMove Game
moveBlackPawn move@(Move from@(Pos row col) to@(Pos row' col')) game =
  if
      | (row' - row) == -1 && abs (col' - col) == 1 ->
          takePiece game from to
      | isJust (game `firstPieceOn` path from to) ->
          Left $ IllegalMove move
      | row <= 5 && row' - row == -1 && col == col' ->
          Right $ movePiece game from to
      | row == 6 && row' - row >= -2 && row' < row && col == col' ->
          Right $ movePiece game from to
      | otherwise ->
          Left $ IllegalMove move
{-# INLINEABLE moveBlackPawn #-}

pieceAt :: Position -> Game -> Maybe PieceOnBoard
pieceAt position Game{pieces} =
  find (\PieceOnBoard{pos} -> pos == position) pieces
{-# INLINEABLE pieceAt #-}

movePiece :: Game -> Position -> Position -> Game
movePiece game@Game{curSide, pieces} from to =
  let
    att = pieceAt from game
    newPos = maybe [] (\PieceOnBoard{piece, side} -> [PieceOnBoard{piece, side, pos = to}]) att
   in
    Game (flipSide curSide) $ filter (\PieceOnBoard{pos} -> pos /= from) pieces <> newPos
{-# INLINEABLE movePiece #-}

takePiece :: Game -> Position -> Position -> Either IllegalMove Game
takePiece game@Game{curSide, pieces} from to =
  let
    att = pieceAt from game
    def = pieceAt to game
    newPos = do
      PieceOnBoard{piece, side} <- att
      PieceOnBoard{side = side'} <- def
      guard $ side /= side'
      pure $ PieceOnBoard piece side to
   in
    case newPos of
      Just p -> Right $ Game (flipSide curSide) $ filter (\PieceOnBoard{pos} -> pos /= from && pos /= to) pieces <> [p]
      Nothing -> Left $ IllegalMove (Move from to)
{-# INLINEABLE takePiece #-}

path :: Position -> Position -> Path
path (Pos r c) (Pos r' c') =
  let vert = r' - r
      horiz = c' - c
   in Path $
        if
            | vert == 0 && horiz > 0 ->
                [Pos r x | x <- enumFromTo (c + 1) c']
            | vert == 0 && horiz < 0 ->
                [Pos r x | x <- enumFromTo c' (c - 1)]
            | horiz == 0 && vert < 0 ->
                [Pos x c | x <- enumFromTo r' (r - 1)]
            | horiz == 0 && vert > 0 ->
                [Pos x c | x <- enumFromTo (r + 1) r']
            | otherwise ->
                []
{-# INLINEABLE path #-}

firstPieceOn :: Game -> Path -> Maybe PieceOnBoard
firstPieceOn Game{pieces} aPath =
  case mapMaybe (\p -> find (\PieceOnBoard{pos} -> p == pos) pieces) $ positions aPath of
    [] -> Nothing
    (a : _) -> Just a
{-# INLINEABLE firstPieceOn #-}

possibleMoves :: Position -> Game -> [Move]
possibleMoves pos@(Pos r c) game =
  let allMoves =
        [ Move pos (Pos r' c')
        | r' <- [0 .. 7]
        , c' <- [0 .. 7]
        , (r, c) /= (r', c')
        ]
   in filter (\move -> isRight $ apply move game) allMoves
{-# INLINEABLE possibleMoves #-}

accessibleDiagonals :: Position -> [Position]
accessibleDiagonals (Pos r c) =
  [ Pos r' c'
  | dr <- [-1, 1]
  , dc <- [-1, 1]
  , (r', c') <- (\ offset -> (r + offset * dr, c + offset * dc)) <$> [1 .. 6]
  , r' >= 0 && r' <= 7 && r' /= r
  , c' >= 0 && c' <= 7 && c' /= c
  ]
{-# INLINEABLE accessibleDiagonals #-}
