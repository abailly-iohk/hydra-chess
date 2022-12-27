{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BlackJack.Client where

import BlackJack.Server (IsParty, Server (..))
import Control.Monad (forM)

data Result p = TableCreated [p]
  deriving stock (Eq, Show)

data Client p m = Client {newTable :: [String] -> m (Result p)}

startClient :: forall p m. (IsParty p, Monad m) => Server p m -> m (Client p m)
startClient server = pure $ Client{newTable}
 where
  newTable ps = do
    parties <- forM ps $ connect server
    initHead server parties
    pure $ TableCreated parties
