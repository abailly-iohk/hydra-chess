{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Games.Server.IO where

import Chess.Render (Render (..))
import Data.Text (unpack)
import Game.Chess (Chess, unMove)
import Game.Server (FromChain (..), IsChain)

notifyChessEvent :: (IsChain c) => FromChain Chess c -> IO ()
notifyChessEvent = \case
  GameStarted{game} -> putStrLn (unpack $ render game)
  GameChanged{game, plays} -> do
    putStrLn (unpack $ render game)
    putStrLn (unlines $ unpack . render . unMove <$> plays)
  other -> print other
