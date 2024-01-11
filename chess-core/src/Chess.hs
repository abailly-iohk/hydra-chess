{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A very simple console-based interface to play chess.
-- This module is mainly useful for testing purpose.
module Chess where
import Chess.Game (initialGame, apply)
import Chess.Render (Render(..))
import Chess.Parse (parseMove)
import qualified Data.Text.IO as Text

-- |Run a Chess game in the console.
--
-- This is a very basic interface, useful to interactively test the
-- rules and game logic.
runGame :: IO ()
runGame =
  go initialGame
   where
     go game = do
       Text.putStrLn $ render game
       getLine >>= handleMove game . parseMove

     handleMove game = \case
         Right  move -> case apply move game of
          Left err -> print err >> go game
          Right game' -> go game'
         Left err -> print err >> go game
