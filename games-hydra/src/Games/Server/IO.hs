{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Games.Server.IO where

import Chess.Game (Move)
import Chess.GameState (ChessGame (..), ChessPlay (..))
import Chess.Render (Render (..))
import Data.Maybe (catMaybes)
import Data.Text (unpack)
import Game.Chess (Chess, unMove)
import Game.Server (FromChain (..), IsChain)

notifyChessEvent :: (IsChain c) => FromChain Chess c -> IO ()
notifyChessEvent = \case
  GameStarted{game = ChessGame{game}} ->
    putStrLn (unpack $ render game)
  GameEnded{game = ChessGame{game}, gameEnd} -> do
    putStrLn (unpack $ render game)
    putStrLn (show gameEnd)
  GameChanged{game = ChessGame{game}, plays} -> do
    putStrLn (unpack $ render game)
    putStrLn (unlines $ unpack . render <$> (catMaybes $ fmap selectMoves $ unMove <$> plays))
  other -> print other

selectMoves :: ChessPlay -> Maybe Move
selectMoves = \case
  ChessMove move -> Just move
  End -> Nothing
