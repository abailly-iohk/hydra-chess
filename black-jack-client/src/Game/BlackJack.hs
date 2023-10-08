{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Game.BlackJack (module BlackJack.Game) where

import BlackJack.Contract.Game (Payoffs)
import BlackJack.Game
import Data.Map (Map)
import Data.Text (Text)
import Game.Server (Game (..))

instance Game BlackJack where
  type GameState BlackJack = BlackJack
  type GamePlay BlackJack = Play
  type GameEnd BlackJack = ([Card], Payoffs, Map Text Integer)
