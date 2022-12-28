{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module BlackJack.Client where

import BlackJack.Server (CommitResult (CommitDone), InitResult (..), IsChain (..), Server (..))
import Control.Monad (forM)
import Control.Monad.Class.MonadThrow (MonadThrow)
import Control.Monad.Class.MonadTimer (MonadDelay, threadDelay)
import Data.Text (Text)

data Result c
  = TableCreated {parties :: [Party c], tableId :: Text}
  | TableCreationFailed {failureReason :: Text}
  | TableFunded {amount :: Coin c, tableId :: Text}

deriving instance IsChain c => Eq (Result c)
deriving instance IsChain c => Show (Result c)

data Client c m = Client
  { newTable :: [Text] -> m (Result c)
  , fundTable :: Text -> Integer -> m (Result c)
  }

startClient ::
  forall c m.
  (IsChain c, Monad m, MonadDelay m, MonadThrow m) =>
  Server c m ->
  m (Client c m)
startClient server = pure $ Client{newTable, fundTable}
 where
  newTable ps = do
    peers <- forM ps $ connect server
    result <- initHead server peers
    let loop =
          result >>= \case
            InitDone{headId} -> pure $ TableCreated{parties = peers, tableId = headId}
            InitPending -> threadDelay 1 >> loop
            InitFailed{reason} -> pure $ TableCreationFailed{failureReason = reason}
    loop

  fundTable tid fund = do
    result <- commit server fund
    result >>= \case
      CommitDone c -> pure $ TableFunded c tid
