{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BlackJack where

import BlackJack.Game (
  BlackJack (..),
  Card,
  Outcome (..),
  Play (..),
  PlayerId,
  newGame,
  play,
  possibleActions,
 )
import BlackJack.Pretty (displayPlayerHand, prettyGame, showAction)
import Control.Exception (IOException, catch)
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO.Error (isEOFError)

runGame :: Int -> Int -> IO ()
runGame numberOfPlayers randomSeed = go $ BlackJack.Game.newGame numberOfPlayers randomSeed
 where
  go game = do
    let actions = Quit : BlackJack.Game.possibleActions game
    displayActions actions
    selectAction actions >>= \case
      Action act ->
        case play game act of
          GameEnds dealers e -> displayEndGame dealers e
          GameContinue g -> displayGame g >> go g
      Error err -> putStrLn err >> go game

selectAction :: [Play] -> IO Input
selectAction choices = do
  selectPlay `catch` \(e :: IOException) ->
    if isEOFError e
      then pure (Action Quit)
      else pure (Error $ show e)
 where
  selectPlay = do
    playerChoice <- reads <$> getLine
    case playerChoice of
      [(choice, "")] ->
        if choice < 0 || choice > (length choices - 1)
          then pure (Error "Invalid choice")
          else pure $ Action (choices !! choice)
      _ -> pure $ Error "Invalid choice"

data Input
  = Action Play
  | Error String
  deriving (Eq, Show)

displayActions :: [Play] -> IO ()
displayActions plays =
  putStrLn $
    unlines $
      "Choose one action:" : fmap showAction (zip [0 ..] plays)

displayGame :: BlackJack -> IO ()
displayGame g = putStrLn $ prettyGame g

displayEndGame :: [Card] -> Map PlayerId Int -> IO ()
displayEndGame dealers gains =
  putStr $
    unlines $
      ("Dealer's Hand: " <> displayPlayerHand dealers) :
      "Payoffs: " : (showGain <$> Map.toList gains)
 where
  showGain (player, amount) = show player <> " : " <> show amount
