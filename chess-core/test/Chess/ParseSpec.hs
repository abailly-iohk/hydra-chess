{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Chess.ParseSpec where

import Chess.Game (Move(..))
import Chess.Parse (parseMove)
import Chess.Render (Render (..))
import Data.Function ((&))
import Data.Text (unpack)
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, counterexample, (===))

spec :: Spec
spec = do
  prop "parses pawn moves" parse_pawn_moves

parse_pawn_moves :: Move -> Property
parse_pawn_moves move =
  let repr = unpack $ render move
   in parseMove repr
        === Right move
        & counterexample ("rendered: " <> repr)
