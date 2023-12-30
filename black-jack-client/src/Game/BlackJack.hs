{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Game.BlackJack (
  module BlackJack.Game,
  BlackJackEnd (..),
) where

import BlackJack.Contract.Game (Payoffs)
import BlackJack.Game
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Bifunctor (first)
import Data.Map (Map, fromList)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Game.Server (Game (..))
import Test.QuickCheck (Arbitrary (arbitrary))

data BlackJackEnd = BlackJackEnd
  { dealerCards :: [Card]
  , payoffs :: Payoffs
  , gains :: Map Text Integer
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary BlackJackEnd where
  arbitrary = BlackJackEnd <$> arbitrary <*> arbitrary <*> someGains
   where
    someGains = fromList . fmap (first (pack . show @PlayerId)) <$> arbitrary

instance Game BlackJack where
  type GameState BlackJack = BlackJack
  newtype GamePlay BlackJack = GamePlay Play
  type GameEnd BlackJack = BlackJackEnd

  initialGame = newGame . fromIntegral

  readPlay = undefined

instance Show (GamePlay BlackJack) where
  show (GamePlay m) = show m

instance Eq (GamePlay BlackJack) where
  (GamePlay m) == (GamePlay m') = m == m'

instance ToJSON (GamePlay BlackJack) where
  toJSON (GamePlay move) = toJSON move

instance FromJSON (GamePlay BlackJack) where
  parseJSON = fmap GamePlay . parseJSON
