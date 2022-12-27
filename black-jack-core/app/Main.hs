module Main where

import BlackJack(runGame)
import System.Environment (getArgs)

main :: IO ()
main = do
  [numPlayers, seed] <- fmap read <$> getArgs
  runGame numPlayers seed
