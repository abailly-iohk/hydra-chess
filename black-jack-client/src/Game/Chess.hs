{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Game.Chess where

import Chess.Game (Move)
import qualified Chess.Game as Chess
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Game.Server (Game (..))

type Chess = Chess.Game

data ChessEnd
  = BlackWins
  | WhiteWins
  | Draw
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Game Chess where
  type GameState Chess = Chess.Game
  type GamePlay Chess = Move
  type GameEnd Chess = ChessEnd
  initialGame = const Chess.initialGame
