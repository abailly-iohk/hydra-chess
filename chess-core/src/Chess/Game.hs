{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialize -fno-strictness -fno-spec-constr -fdefer-type-errors #-}

module Chess.Game where

import PlutusTx.Prelude

import Control.Monad (guard)
import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)
import qualified PlutusTx
import Test.QuickCheck (Arbitrary (..), choose, suchThat)
import qualified Prelude
import qualified Prelude as Haskell

type Row = Integer
type Col = Integer

data Position = Pos {row :: Row, col :: Col}
  deriving (Haskell.Eq, Haskell.Show, Generic, ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''Position

instance Arbitrary Position where
  arbitrary = Pos Prelude.<$> choose (0, 7) Prelude.<*> choose (0, 7)

instance Eq Position where
  Pos r c == Pos r' c' = r == r' && c == c'

newtype Path = Path {positions :: [Position]}
  deriving (Haskell.Eq, Haskell.Show)

emptyPath :: Path
emptyPath = Path []

instance Eq Path where
  Path p == Path p' = p == p'

data Piece = Pawn | Rook | Bishop | Knight | Queen | King
  deriving (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''Piece

instance Eq Piece where
  Pawn == Pawn = True
  Rook == Rook = True
  Bishop == Bishop = True
  Knight == Knight = True
  Queen == Queen = True
  King == King = True
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

data Check = NoCheck | Check Side | CheckMate Side
  deriving (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''Check

instance Eq Check where
  NoCheck == NoCheck = True
  Check side == Check side' = side == side'
  CheckMate side == CheckMate side' = side == side'
  _ == _ = False

data Game = Game
  { curSide :: Side
  , checkState :: Check
  , pieces :: [PieceOnBoard]
  }
  deriving (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''Game

instance Eq Game where
  Game s c p == Game s' c' p' = s == s' && p == p' && c == c'

mkGame :: Side -> [PieceOnBoard] -> Game
mkGame curSide pieces =
  Game{curSide, pieces, checkState = NoCheck}

initialGame :: Game
initialGame =
  Game
    { curSide = White
    , checkState = NoCheck
    , pieces =
        [PieceOnBoard Pawn White (Pos 1 c) | c <- [0 .. 7]]
          <> [PieceOnBoard Pawn Black (Pos 6 c) | c <- [0 .. 7]]
          <> [PieceOnBoard Rook Black (Pos 7 0), PieceOnBoard Rook Black (Pos 7 7)]
          <> [PieceOnBoard Rook White (Pos 0 0), PieceOnBoard Rook White (Pos 0 7)]
          <> [PieceOnBoard Knight Black (Pos 7 1), PieceOnBoard Knight Black (Pos 7 6)]
          <> [PieceOnBoard Knight White (Pos 0 1), PieceOnBoard Knight White (Pos 0 6)]
          <> [PieceOnBoard Bishop Black (Pos 7 2), PieceOnBoard Bishop Black (Pos 7 5)]
          <> [PieceOnBoard Bishop White (Pos 0 2), PieceOnBoard Bishop White (Pos 0 5)]
          <> [PieceOnBoard Queen Black (Pos 7 3)]
          <> [PieceOnBoard Queen White (Pos 0 3)]
          <> [PieceOnBoard King Black (Pos 7 4)]
          <> [PieceOnBoard King White (Pos 0 4)]
    }

findPieces :: Piece -> Side -> Game -> [PieceOnBoard]
findPieces piece' side' Game{pieces} =
  filter (\PieceOnBoard{piece, side} -> piece == piece' && side == side') pieces

isCheck :: Side -> Game -> Bool
isCheck side = \case
  Game{checkState = Check side'} -> side == side'
  _ -> False

isCheckMate :: Side -> Game -> Bool
isCheckMate side = \case
  Game{checkState = CheckMate side'} -> side == side'
  _ -> False

isEndGame :: Game -> Bool
isEndGame Game{checkState} =
  case checkState of
    CheckMate{} -> True
    _ -> False

data Move = Move Position Position
  deriving (Haskell.Eq, Haskell.Show, Generic, ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''Move

instance Arbitrary Move where
  arbitrary = do
    from <- arbitrary
    to <- arbitrary `suchThat` (/= from)
    Prelude.pure $ Move from to

data IllegalMove
  = NotMoving Move
  | IllegalMove Move
  | WrongSideToPlay Side Move
  | MoveBlocked Position Move
  | StillInCheck Side
  | GameEnded Check
  deriving (Haskell.Eq, Haskell.Show)

PlutusTx.unstableMakeIsData ''IllegalMove

apply :: Move -> Game -> Either IllegalMove Game
apply move game =
  doMove move game >>= updateCheckState
{-# INLINEABLE apply #-}

updateCheckState :: Game -> Either IllegalMove Game
updateCheckState game@Game{curSide, checkState} =
  ensureCheckIsRemoved >>= changeCheckState
 where
  sidePlaying = flipSide curSide

  ensureCheckIsRemoved =
    if checkState == Check sidePlaying && isInCheck sidePlaying game
      then Left $ StillInCheck sidePlaying
      else Right $ game{checkState = NoCheck}

  changeCheckState game' =
    if isInCheck curSide game'
      then
        if isInCheckMate curSide game'
          then Right $ game'{checkState = CheckMate curSide}
          else Right $ game'{checkState = Check curSide}
      else Right game'
{-# INLINEABLE updateCheckState #-}

isInCheck :: Side -> Game -> Bool
isInCheck checkedSide game@Game{pieces} =
  let findKing = find (\PieceOnBoard{piece, side} -> piece == King && side == checkedSide) pieces
   in case findKing of
        Nothing -> False -- TODO: this should not happen in a real game?
        Just king -> any (canTake game king) pieces
{-# INLINEABLE isInCheck #-}

isInCheckMate :: Side -> Game -> Bool
isInCheckMate checkedSide game@Game{pieces} =
  let allPieces = filter (\PieceOnBoard{side} -> side == checkedSide) pieces
      moves =
        rights
          . map (`doMove` game)
          . foldMap ((`legalMoves` game) . pos)
          $ allPieces
   in all (isInCheck checkedSide) moves
{-# INLINEABLE isInCheckMate #-}

rights :: [Either IllegalMove Game] -> [Game]
rights = foldr selectRight []
 where
  selectRight :: Either IllegalMove Game -> [Game] -> [Game]
  selectRight move games = case move of
    Left _ -> games
    Right game -> game : games
{-# INLINEABLE rights #-}

canTake :: Game -> PieceOnBoard -> PieceOnBoard -> Bool
canTake game PieceOnBoard{pos = king} PieceOnBoard{side, pos = other} =
  isRight $ doMove (Move other king) (game{curSide = side})
{-# INLINEABLE canTake #-}

doMove :: Move -> Game -> Either IllegalMove Game
doMove move@(Move from to) game@Game{checkState, curSide}
  | isEndGame game = Left $ GameEnded checkState
  | from == to = Left $ NotMoving move
  | otherwise =
      case pieceAt from game of
        Just (PieceOnBoard Pawn White _) | curSide == White -> moveWhitePawn move game
        Just (PieceOnBoard Pawn Black _) | curSide == Black -> moveBlackPawn move game
        Just (PieceOnBoard Rook side _) | curSide == side -> moveRook move game
        Just (PieceOnBoard Knight side _) | curSide == side -> moveKnight move game
        Just (PieceOnBoard Bishop side _) | curSide == side -> moveBishop move game
        Just (PieceOnBoard King side _) | curSide == side -> moveKing move game
        Just (PieceOnBoard Queen side _)
          | curSide == side ->
              either (const $ moveBishop move game) Right $ moveRook move game
        _ -> Left $ WrongSideToPlay curSide move
{-# INLINEABLE doMove #-}

moveKing :: Move -> Game -> Either IllegalMove Game
moveKing move@(Move (Pos row col) (Pos row' col')) game =
  if
      | abs (row' - row) <= 1 && abs (col' - col) <= 1 ->
          moveOrTakePiece move game
      | otherwise ->
          Left $ IllegalMove move
{-# INLINEABLE moveKing #-}

moveRook :: Move -> Game -> Either IllegalMove Game
moveRook move@(Move (Pos row col) (Pos row' col')) game =
  if
      | row' == row || col' == col ->
          moveOrTakePiece move game
      | otherwise ->
          Left $ IllegalMove move
{-# INLINEABLE moveRook #-}

moveKnight :: Move -> Game -> Either IllegalMove Game
moveKnight move@(Move from@(Pos row col) to@(Pos row' col')) game =
  if
      | (abs (row' - row) == 1 && abs (col' - col) == 2)
          || (abs (row' - row) == 2 && abs (col' - col) == 1) ->
          if isJust (pieceAt to game)
            then takePiece game from to
            else Right $ movePiece game from to
      | otherwise ->
          Left $ IllegalMove move
{-# INLINEABLE moveKnight #-}

moveBishop :: Move -> Game -> Either IllegalMove Game
moveBishop move@(Move (Pos row col) (Pos row' col')) game =
  if
      | abs (row' - row) == abs (col' - col) ->
          moveOrTakePiece move game
      | otherwise ->
          Left $ IllegalMove move
{-# INLINEABLE moveBishop #-}

moveOrTakePiece :: Move -> Game -> Either IllegalMove Game
moveOrTakePiece move@(Move from to) game =
  case game `firstPieceOn` path from to of
    Just (PieceOnBoard{pos})
      | pos == to -> takePiece game from to
      | otherwise -> Left $ MoveBlocked pos move
    Nothing -> Right $ movePiece game from to
{-# INLINEABLE moveOrTakePiece #-}

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
    game
      { curSide = flipSide curSide
      , pieces = filter (\PieceOnBoard{pos} -> pos /= from) pieces <> newPos
      }
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
      Just p ->
        Right $
          game
            { curSide = flipSide curSide
            , pieces = filter (\PieceOnBoard{pos} -> pos /= from && pos /= to) pieces <> [p]
            }
      Nothing -> Left $ IllegalMove (Move from to)
{-# INLINEABLE takePiece #-}

path :: Position -> Position -> Path
path (Pos r c) (Pos r' c') =
  if
      | abs vert == abs horiz -> diagonalPath
      | r == r' || c == c' -> orthogonalPath
      | otherwise -> emptyPath
 where
  vert = r' - r
  horiz = c' - c
  diagonalPath =
    Path $
      if
          | vert > 0 && horiz > 0 ->
              [Pos (r + x) (c + x) | x <- enumFromTo 1 (abs horiz)]
          | vert > 0 && horiz < 0 ->
              [Pos (r + x) (c - x) | x <- enumFromTo 1 (abs horiz)]
          | vert < 0 && horiz < 0 ->
              [Pos (r - x) (c - x) | x <- enumFromTo 1 (abs horiz)]
          | vert < 0 && horiz > 0 ->
              [Pos (r - x) (c + x) | x <- enumFromTo 1 (abs horiz)]
          | otherwise ->
              []
  orthogonalPath =
    Path $
      if
          | vert == 0 && horiz > 0 ->
              [Pos r x | x <- enumFromTo (c + 1) c']
          | vert == 0 && horiz < 0 ->
              [Pos r x | x <- reverse $ enumFromTo c' (c - 1)]
          | horiz == 0 && vert < 0 ->
              [Pos x c | x <- reverse $ enumFromTo r' (r - 1)]
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

legalMoves :: Position -> Game -> [Move]
legalMoves pos@(Pos r c) game =
  let allMoves =
        [ Move pos (Pos r' c')
        | r' <- enumFromTo 0 7
        , c' <- enumFromTo 0 7
        , (r, c) /= (r', c')
        ]
   in filter (\move -> isRight $ doMove move game) allMoves
{-# INLINEABLE legalMoves #-}

possibleMoves :: Position -> Game -> [Move]
possibleMoves pos@(Pos r c) game =
  let allMoves =
        [ Move pos (Pos r' c')
        | r' <- [0 .. 7]
        , c' <- [0 .. 7]
        , (r, c) /= (r', c')
        ]
   in filter (\move -> isRight $ apply move game) allMoves

accessibleDiagonals :: Position -> [Position]
accessibleDiagonals (Pos r c) =
  [ Pos r' c'
  | dr <- [-1, 1]
  , dc <- [-1, 1]
  , (r', c') <- (\offset -> (r + offset * dr, c + offset * dc)) <$> [1 .. 6]
  , inBounds r' c'
  , r' /= r && c' /= c
  ]
{-# INLINEABLE accessibleDiagonals #-}

accessibleOrthogonally :: Position -> [Position]
accessibleOrthogonally (Pos r c) =
  [ Pos r' c
  | r' <- [0 .. 7]
  , r' /= r
  ]
    <> [ Pos r c'
       | c' <- [0 .. 7]
       , c' /= c
       ]
{-# INLINEABLE accessibleOrthogonally #-}

accessibleByKnight :: Position -> [Position]
accessibleByKnight (Pos r c) =
  [ Pos r' c'
  | x <- [-2, 2]
  , y <- [-1, 1]
  , (r', c') <- [(r + x, c + y), (r + y, c + x)]
  , inBounds r' c'
  , r' /= r && c' /= c
  ]
{-# INLINEABLE accessibleByKnight #-}

inBounds :: Row -> Col -> Bool
inBounds r c =
  r >= 0 && r <= 7 && c >= 0 && c <= 7
{-# INLINEABLE inBounds #-}

adjacentTo :: Position -> [Position]
adjacentTo (Pos r c) =
  [ Pos r' c'
  | dr <- [-1, 0, 1]
  , dc <- [-1, 0, 1]
  , let r' = r + dr
  , let c' = c + dc
  , inBounds r' c'
  , (r', c') /= (r, c)
  ]
{-# INLINEABLE adjacentTo #-}

notOrthogonalTo :: Position -> Position -> Bool
notOrthogonalTo (Pos c r) (Pos c' r') = c /= c' && r /= r'
{-# INLINEABLE notOrthogonalTo #-}
