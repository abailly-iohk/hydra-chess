module Chess.Game where

apply :: Move -> Game -> Bool
apply _move _game = undefined

data Position = Pos Row Col
  deriving (Eq, Show)

type Row = Int
type Col = Int

data Game = Game
 deriving (Eq, Show)

initialGame :: Game
initialGame = undefined

data Side=  White | Black
  deriving (Eq, Show)

data Move = Move Position Position
  deriving (Eq, Show)
