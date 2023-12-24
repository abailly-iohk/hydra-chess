{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Chess.Generators where

import Chess.Game
import Control.Monad.State (MonadState (..), StateT, execStateT, lift)
import Test.QuickCheck (Arbitrary (..), Gen, choose, elements)

-- * Generators

generateMove :: Position -> Game -> Gen Move
generateMove pos game =
  case possibleMoves pos game of
    [] -> error $ "no possible moves from " <> show pos <> "in game " <> show game
    other -> elements other

anyValidPawn :: Side -> Gen Position
anyValidPawn _ =
  elements [Pos r c | r <- [1 .. 6], c <- [0 .. 7]]

anyPawn :: Side -> Game -> Gen Position
anyPawn side game =
  elements ((\PieceOnBoard{pos} -> pos) <$> findPieces Pawn side game)

anyPos :: Gen Position
anyPos =
  Pos <$> anyRow <*> anyColumn

anyColumn :: Gen Integer
anyColumn =
  elements [0 .. 7]

anyRow :: Gen Integer
anyRow =
  elements [0 .. 7]

instance Arbitrary Piece where
  arbitrary = elements [Pawn, Rook, Bishop, Knight]

instance Arbitrary Side where
  arbitrary = elements [White, Black]

instance Arbitrary Game where
  arbitrary = execStateT genMoves initialGame

genMoves :: StateT Game Gen ()
genMoves =
  lift (choose (0, 30)) >>= genMove
 where
  genMove :: Int -> StateT Game Gen ()
  genMove 0 = pure ()
  genMove n = do
    game@Game{pieces} <- get
    case concatMap ((`possibleMoves` game) . pos) pieces of
      [] -> pure ()
      moves -> do
        move <- lift $ elements moves
        either (const $ pure ()) put (apply move game)
        genMove (n - 1)
