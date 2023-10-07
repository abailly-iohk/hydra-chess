{-# LANGUAGE MultiWayIf #-}

module Chess.Game where

import Data.Foldable (find)
import Data.Maybe (isJust)

apply :: Move -> Game -> Either IllegalMove Game
apply move@(Move from@(Pos row col) to@(Pos row' col')) game
  | (row' - row) == 1 && abs (col' - col) == 1 =
      Right $ takePiece game from to
  | game `hasPieceOn` path from to =
      Left $ IllegalMove move
  | row >= 2 && row' - row == 1 =
      Right $ movePiece game from to
  | row == 1 && row' - row <= 2 =
      Right $ movePiece game from to
  | otherwise =
      Left $ IllegalMove move

movePiece :: Game -> Position -> Position -> Game
movePiece (Game pieces) from to =
  let
    att = find (\(_, _, pos) -> pos == from) pieces
    newPos = maybe [] (\(p, s, _) -> [(p, s, to)]) att
   in
    Game $ filter (\(_, _, pos) -> pos /= from) pieces <> newPos

takePiece :: Game -> Position -> Position -> Game
takePiece (Game pieces) from to =
  let
    att = find (\(_, _, pos) -> pos == from) pieces
    newPos = maybe [] (\(p, s, _) -> [(p, s, to)]) att
   in
    Game $ filter (\(_, _, pos) -> pos /= from && pos /= to) pieces <> newPos

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
            | otherwise -> error "non linear move"

hasPieceOn :: Game -> Path -> Bool
hasPieceOn (Game pieces) =
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

data Game = Game [(Piece, Side, Position)]
  deriving (Eq, Show)

initialGame :: Game
initialGame =
  Game $
    [(Pawn, White, Pos 1 c) | c <- [0 .. 7]]
      <> [(Pawn, Black, Pos 6 c) | c <- [0 .. 7]]

findPieces :: Piece -> Side -> Game -> [(Piece, Side, Position)]
findPieces piece side (Game pieces) =
  filter (\(p, s, _) -> p == piece && s == side) pieces

data Side = White | Black
  deriving (Eq, Show)

data Move = Move Position Position
  deriving (Eq, Show)
