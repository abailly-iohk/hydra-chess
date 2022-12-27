{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BlackJack.Client where

import BlackJack.Server (InitResult (..), IsParty, Server (..))
import Control.Monad (forM)
import Control.Monad.Class.MonadTimer (MonadDelay, threadDelay)

data Result p = TableCreated [p]
  deriving stock (Eq, Show)

data Client p m = Client {newTable :: [String] -> m (Result p)}

startClient :: forall p m. (IsParty p, Monad m, MonadDelay m) => Server p m -> m (Client p m)
startClient server = pure $ Client{newTable}
 where
  newTable ps = do
    parties <- forM ps $ connect server
    result <- initHead server parties
    let loop =
          result >>= \case
            InitDone -> pure ()
            InitPending -> threadDelay 1 >> loop
            InitFailed _ -> pure ()
    loop
    pure $ TableCreated parties
