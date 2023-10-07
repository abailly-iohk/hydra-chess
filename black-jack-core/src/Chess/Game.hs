{-# LANGUAGE MultiWayIf #-}

module Chess.Game where

import Data.Foldable (find)
import Data.Maybe (isJust)

apply :: Move -> Game -> Either IllegalMove Game
apply move@(Move from@(Pos row _) to@(Pos row' _)) game
  | game `hasPieceOn` path from to = Left $ IllegalMove move
  | row >= 2 && row' - row == 1 = Right $ Game [(Pawn, to)]
  | row == 1 && row' - row <= 2 = Right $ Game [(Pawn, to)]
  | otherwise = Left $ IllegalMove move

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
  any (\pos -> isJust $ find ((== pos) . snd) pieces) . positions

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

data Game = Game [(Piece, Position)]
  deriving (Eq, Show)

initialGame :: Game
initialGame = Game []

data Side = White | Black
  deriving (Eq, Show)

data Move = Move Position Position
  deriving (Eq, Show)
