module BlackJack.Pretty where

import BlackJack.Game (
  BlackJack (..),
  Card (..),
  Color (..),
  DealerHand (DealerHand),
  Face (..),
  Play (..),
  Player (..),
  PlayerId,
  handValues,
 )
import qualified Data.Map as Map

prettyGame :: BlackJack -> String
prettyGame (Setup n _ playerBets) =
  let bet (k, v) = " " <> show k <> " ↦ " <> show v
      output = unlines $ ("Players: " <> show n) : (bet <$> Map.toList playerBets)
   in output
prettyGame (BlackJack _ _ nextPlayer dealer playersState) =
  let output = unlines $ ("Next: " <> show nextPlayer) : displayDealerHand dealer : (displayPlayer <$> Map.toList playersState)
   in output

displayDealerHand :: DealerHand -> String
displayDealerHand (DealerHand (_, cards)) = " Dealer: " <> unwords ("??" : (displayCard <$> cards))

displayCard :: Card -> String
displayCard (Card co fa) = displayFace fa <> displayColor co

displayColor :: Color -> String
displayColor Heart = "\x2661"
displayColor Spade = "\x2660"
displayColor Diamond = "\x2662"
displayColor Club = "\x2663"

displayFace :: Face -> String
displayFace Two = "\x1f0a2"
displayFace Three = "\x1f0a3"
displayFace Four = "\x1f0a4"
displayFace Five = "\x1f0a5"
displayFace Six = "\x1f0a6"
displayFace Seven = "\x1f0a7"
displayFace Eight = "\x1f0a8"
displayFace Nine = "\x1f0a9"
displayFace Ten = "\x1f0aa"
displayFace Jack = "\x1f0ab"
displayFace Queen = "\x1f0ac"
displayFace King = "\x1f0ad"
displayFace Ace = "\x1f0a1"

displayPlayer :: (PlayerId, Player) -> String
displayPlayer (pid, p) =
  " " <> show pid <> ": " <> displayPlayerState p

displayPlayerState :: Player -> [Char]
displayPlayerState (Playing cas n) = "(" <> show n <> ") " <> displayPlayerHand cas
displayPlayerState (Standing cas n) = "[" <> show n <> "] " <> displayPlayerHand cas

displayPlayerHand :: [Card] -> String
displayPlayerHand cards = unwords (show (handValues cards) : (displayCard <$> cards))

showAction :: (Int, Play) -> String
showAction (idx, p) = show idx <> " - " <> show p
