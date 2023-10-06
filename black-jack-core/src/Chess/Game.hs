module Chess.Game where

apply :: Move -> Game -> Either IllegalMove Game
apply (Move _ to) _game =
  Right $ Game [(Pawn, to)]

data IllegalMove = IllegalMove Move
  deriving (Eq, Show)

data Position = Pos Row Col
  deriving (Eq, Show)

type Row = Int
type Col = Int

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
