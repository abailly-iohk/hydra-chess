{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BlackJack.Client where

import BlackJack.Server (InitResult (..), IsParty, Server (..))
import Control.Monad (forM)
import Control.Monad.Class.MonadThrow (MonadThrow)
import Control.Monad.Class.MonadTimer (MonadDelay, threadDelay)
import Data.Text (Text)

data Result p
  = TableCreated {parties :: [p], tableId :: Text}
  | TableCreationFailed {failureReason :: Text}
  | TableFunded {amount :: Integer, tableId :: Text}
  deriving stock (Eq, Show)

data Client p m = Client
  { newTable :: [Text] -> m (Result p)
  , fundTable :: Text -> Integer -> m (Result p)
  }

startClient :: forall p m. (IsParty p, Monad m, MonadDelay m, MonadThrow m) => Server p m -> m (Client p m)
startClient server = pure $ Client{newTable, fundTable}
 where
  newTable ps = do
    parties <- forM ps $ connect server
    result <- initHead server parties
    let loop =
          result >>= \case
            InitDone{headId} -> pure $ TableCreated{parties, tableId = headId}
            InitPending -> threadDelay 1 >> loop
            InitFailed{reason} -> pure $ TableCreationFailed{failureReason = reason}
    loop

  fundTable tid fund =
    pure $ TableFunded fund tid
