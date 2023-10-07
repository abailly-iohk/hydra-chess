{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}

module Chess.Game where

import Control.Monad (guard)
import Data.Foldable (find)
import Data.Maybe (isJust)

apply :: Move -> Game -> Either IllegalMove Game
apply move@(Move from _) game@(Game curSide _) =
  case pieceAt from game of
    Just (Pawn, White, _) | curSide == White -> moveWhitePawn move game
    Just (Pawn, Black, _) | curSide == Black -> moveBlackPawn move game
    _ -> Left $ IllegalMove move

moveWhitePawn :: Move -> Game -> Either IllegalMove Game
moveWhitePawn move@(Move from@(Pos row col) to@(Pos row' col')) game
  | (row' - row) == 1 && abs (col' - col) == 1 =
      takePiece game from to
  | game `hasPieceOn` path from to =
      Left $ IllegalMove move
  | row >= 2 && row' - row == 1 && col == col' =
      Right $ movePiece game from to
  | row == 1 && row' - row <= 2 && row' > row && col == col' =
      Right $ movePiece game from to
  | otherwise =
      Left $ IllegalMove move

moveBlackPawn :: Move -> Game -> Either IllegalMove Game
moveBlackPawn move@(Move from@(Pos row col) to@(Pos row' col')) game
  | (row' - row) == -1 && abs (col' - col) == 1 =
      takePiece game from to
  | game `hasPieceOn` path from to =
      Left $ IllegalMove move
  | row <= 5 && row' - row == -1 && col == col' =
      Right $ movePiece game from to
  | row == 6 && row' - row >= -2 && row' < row && col == col' =
      Right $ movePiece game from to
  | otherwise =
      Left $ IllegalMove move

pieceAt :: Position -> Game -> Maybe (Piece, Side, Position)
pieceAt position Game{pieces} =
  find (\(_, _, p) -> p == position) pieces

movePiece :: Game -> Position -> Position -> Game
movePiece game@Game{curSide, pieces} from to =
  let
    att = pieceAt from game
    newPos = maybe [] (\(p, s, _) -> [(p, s, to)]) att
   in
    Game (flipSide curSide) $ filter (\(_, _, pos) -> pos /= from) pieces <> newPos

takePiece :: Game -> Position -> Position -> Either IllegalMove Game
takePiece game@Game{curSide, pieces} from to =
  let
    att = pieceAt from game
    def = pieceAt to game
    newPos = do
      (piece, side, _) <- att
      (_, side', _) <- def
      guard $ side /= side'
      pure (piece, side, to)
   in
    case newPos of
      Just pos -> Right $ Game (flipSide curSide) $ filter (\(_, _, p) -> p /= from && p /= to) pieces <> [pos]
      Nothing -> Left $ IllegalMove (Move from to)

path :: Position -> Position -> Path
path (Pos r c) (Pos r' c') =
  let vert = r' - r
      horiz = c' - c
   in Path $
        if
            | vert == 0 && horiz > 0 -> [Pos r x | x <- [c + 1 .. c']]
            | vert == 0 && horiz < 0 -> [Pos r x | x <- [c - 1 .. c']]
            | horiz == 0 && vert < 0 -> [Pos x c | x <- [r - 1 .. r']]
            | horiz == 0 && vert > 0 -> [Pos x c | x <- [r + 1 .. r']]
            | otherwise -> []

hasPieceOn :: Game -> Path -> Bool
hasPieceOn Game{pieces} =
  any (\pos -> isJust $ find (\(_, _, p) -> p == pos) pieces) . positions

data IllegalMove = IllegalMove Move
  deriving (Eq, Show)

data Position = Pos Row Col
  deriving (Eq, Show)

type Row = Int
type Col = Int

newtype Path = Path {positions :: [Position]}
  deriving (Eq, Show)

data Piece = Pawn
  deriving (Eq, Show)

data Game = Game
  { curSide :: Side
  , pieces :: [(Piece, Side, Position)]
  }
  deriving (Eq, Show)

initialGame :: Game
initialGame =
  Game White $
    [(Pawn, White, Pos 1 c) | c <- [0 .. 7]]
      <> [(Pawn, Black, Pos 6 c) | c <- [0 .. 7]]

findPieces :: Piece -> Side -> Game -> [(Piece, Side, Position)]
findPieces piece side Game{pieces} =
  filter (\(p, s, _) -> p == piece && s == side) pieces

data Side = White | Black
  deriving (Eq, Show)

flipSide :: Side -> Side
flipSide White = Black
flipSide Black = White

data Move = Move Position Position
  deriving (Eq, Show)
