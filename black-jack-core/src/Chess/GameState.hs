{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialize -fdefer-type-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}
module Chess.GameState where

import Prelude(($), fmap, pure, (<$>), Bool (..))
import Chess.Plutus (pubKeyHashFromHex, pubKeyHashToHex)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import GHC.Generics (Generic)
import PlutusLedgerApi.V2 (PubKeyHash)
import qualified PlutusTx
import qualified Prelude as Haskell
import Chess.Game (Game, Move)

-- * Toplevel state/transition
data ChessGame = ChessGame
  { players :: [PubKeyHash]
  -- ^ Players identified by their pkh.
  -- Their might be one or 2 players at start and by convention the one whose
  -- pkh is closest to the starting game's hash value (??) is `White`, the other
  -- being `Black`.
  , game :: Game
  -- ^ State of the game.
  }
  deriving (Haskell.Eq, Haskell.Show, Generic)

PlutusTx.unstableMakeIsData ''ChessGame

instance ToJSON ChessGame where
  toJSON ChessGame{players, game} =
    object
      [ "game" .= game
      , "players" .= (pubKeyHashToHex <$> players)
      ]

instance FromJSON ChessGame where
  parseJSON = withObject "ChessGame" $ \obj -> do
    game <- obj .: "game"
    players <- fmap pubKeyHashFromHex <$> obj .: "players"
    pure ChessGame{game, players}

data ChessPlay
  = -- | A normal move.
    ChessMove Move
  | -- | End game.
    -- Only valid if the `game` has ended through a `Draw` or a `CheckMate`.
    -- If it's the case, then the player, identified by their pkh from the transaction's
    -- signatories, can collect their "reward".
    End
  deriving (Haskell.Eq, Haskell.Show, Generic, ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''ChessPlay

isMove :: ChessPlay -> Bool
isMove = \case
  ChessMove{} -> True
  End -> False
