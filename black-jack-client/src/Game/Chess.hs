{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Game.Chess where

import qualified Chess.Game as Chess
import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)
import Game.Server (Game (..))
import Chess.Parse (parseMove)
import Data.Text (unpack)
import Chess.GameState(ChessGame(..), ChessPlay(..))

type Chess = Chess.Game

data ChessEnd
  = BlackWins
  | WhiteWins
  | Draw
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Game Chess where
  type GameState Chess = ChessGame
  newtype GamePlay Chess = GamePlay { unMove :: ChessPlay }
  type GameEnd Chess = ChessEnd
  initialGame = const $ ChessGame { game = Chess.initialGame, players = []}
  readPlay =
    either (const Nothing) (Just . GamePlay . ChessMove) . parseMove . unpack


instance Show (GamePlay Chess) where
  show (GamePlay m) = show m

instance Eq (GamePlay Chess) where
  (GamePlay m) == (GamePlay m')  = m == m'

instance ToJSON (GamePlay Chess) where
  toJSON (GamePlay move) = toJSON move

instance FromJSON (GamePlay Chess) where
  parseJSON = fmap GamePlay . parseJSON
