{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BlackJack where

import BlackJack.Contract.Game (BlackJack, Card, Outcome (..), Play (Quit), PlayerId, possibleActions)
import BlackJack.Game (
  newGame,
  play,
 )
import BlackJack.Pretty (displayPlayerHand, prettyGame, showAction)
import Control.Exception (IOException, catch)
import Control.Monad.State (runState)
import qualified PlutusTx.AssocMap as AssocMap
import System.IO.Error (isEOFError)
import System.Random (mkStdGen)

runGame :: Int -> Int -> IO ()
runGame numberOfPlayers randomSeed =
  go (mkStdGen randomSeed) $ newGame (fromIntegral numberOfPlayers)
 where
  go seed game = do
    let actions = Quit : possibleActions game
    displayActions actions
    selectAction actions >>= \case
      Action act ->
        case runState (play game act) seed of
          (GameEnds dealers e, _) -> displayEndGame dealers (AssocMap.toList e)
          (GameContinue g, seed') -> displayGame g >> go seed' g
      Error err -> putStrLn err >> go seed game

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

displayEndGame :: [Card] -> [(PlayerId, Integer)] -> IO ()
displayEndGame dealers gains =
  putStr $
    unlines $
      ("Dealer's Hand: " <> displayPlayerHand dealers) :
      "Payoffs: " : (showGain <$> gains)
 where
  showGain (player, amount) = show player <> " : " <> show amount
