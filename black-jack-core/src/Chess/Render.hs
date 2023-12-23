{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Chess.Render where

import Chess.Game (
  Game (..),
  Piece (..),
  PieceOnBoard (PieceOnBoard),
  Position (..),
  Side (..),
  pieceAt, Move (..),
 )
import Data.Char (intToDigit, chr, ord)
import Data.Text (Text, pack)
import qualified Data.Text as Text

class Render r where
  render :: r -> Text

instance Render Position where
  render (Pos r c) = pack [chr (ord 'a' + fromInteger c), chr (ord '1' + fromInteger r)]

instance Render Move where
  render (Move f t) = render f <> "-" <> render t

instance Render Game where
  render game =
    let allPos = [character (pieceAt (Pos r c) game) | r <- [0 .. 7], c <- [0 .. 7]]
        raws = reverse $ splitEvery 8 allPos
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
  Just (PieceOnBoard Rook White _) -> '♖'
  Just (PieceOnBoard Rook Black _) -> '♜'
  Nothing -> ' '
