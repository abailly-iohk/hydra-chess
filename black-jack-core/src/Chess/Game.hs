module Chess.Game where
import Data.Foldable (find)
import Data.Maybe (isJust)

apply :: Move -> Game -> Either IllegalMove Game
apply move@(Move (Pos row _) to@(Pos row' _)) game
   | game `hasPieceIn` to =  Left $ IllegalMove move
   | row >= 2 && row' - row == 1 = Right $ Game [(Pawn, to)]
   | row == 1 && row' - row <= 2 = Right $ Game [(Pawn, to)]
   | otherwise = Left $ IllegalMove move

hasPieceIn :: Game -> Position -> Bool
hasPieceIn (Game pieces) pos =
  isJust $ find ((== pos) . snd) pieces

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
