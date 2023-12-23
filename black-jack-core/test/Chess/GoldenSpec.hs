{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NamedFieldPuns #-}

module Chess.GoldenSpec where

import Chess.Game (Game (..), initialGame, possibleMoves, PieceOnBoard (..), apply)
import Control.Monad.State (StateT, execStateT, MonadState (..), lift)
import Test.Aeson.GenericSpecs (Proxy (Proxy), roundtripAndGoldenSpecs)
import Test.Hspec (Spec)
import Test.QuickCheck (Arbitrary (..), Gen, elements, choose)

spec :: Spec
spec = do
  roundtripAndGoldenSpecs (Proxy @Game)

instance Arbitrary Game where
  arbitrary = execStateT genMoves initialGame

genMoves :: StateT Game Gen ()
genMoves =
  lift (choose (0, 30)) >>= genMove
 where
   genMove :: Int -> StateT Game Gen ()
   genMove 0 = pure ()
   genMove n = do
     game@Game{pieces} <- get
     case concatMap ((`possibleMoves` game) . pos) pieces of
       [] -> pure ()
       moves ->  do
         move <- lift $ elements moves
         either (const $ pure ()) put (apply move game)
         genMove (n-1)
