module Chess.Game where

apply :: Move -> Game -> Either IllegalMove Game
apply _move game = Right game

data IllegalMove = IllegalMove Move
  deriving (Eq, Show)

data Position = Pos Row Col
  deriving (Eq, Show)

type Row = Int
type Col = Int

data Game = Game
  deriving (Eq, Show)

initialGame :: Game
initialGame = Game

data Side = White | Black
  deriving (Eq, Show)

data Move = Move Position Position
  deriving (Eq, Show)
