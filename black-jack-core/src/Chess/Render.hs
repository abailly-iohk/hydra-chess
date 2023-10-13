{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Chess.Render where

import Chess.Game (
  Game (..),
  Piece (..),
  PieceOnBoard (PieceOnBoard),
  Position (..),
  Side (..),
  pieceAt,
 )
import Data.Char (intToDigit)
import Data.Text (Text)
import qualified Data.Text as Text

render :: Game -> Text
render game =
  let allPos = [character (pieceAt (Pos r c) game) | r <- [0 .. 7], c <- [0 .. 7]]
      raws = splitEvery 8 allPos
      rows = zipWith (\cs n -> intToDigit n : ' ' : cs) raws [8, 7 .. 1]
   in Text.unlines $ (Text.pack <$> rows) <> ["  abcdefgh"]

splitEvery :: Int -> [Char] -> [String]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs
 where
  (as, bs) = splitAt n xs

character :: Maybe PieceOnBoard -> Char
character = \case
  Just (PieceOnBoard Pawn White _) -> '♙'
  Just (PieceOnBoard Pawn Black _) -> '♟'
  Nothing -> ' '
