{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module BlackJack.Client where

import BlackJack.Client.IO (Command (..), Err (..), HasIO (..), Output (Bye))
import BlackJack.Server (CommitResult (..), InitResult (..), IsChain (..), Server (..))
import Control.Monad.Class.MonadThrow (MonadThrow)
import Control.Monad.Class.MonadTimer (MonadDelay, threadDelay)
import Data.Text (Text, pack)

data Result c
  = TableCreated {parties :: [Party c], tableId :: Text}
  | TableCreationFailed {failureReason :: Text}
  | TableFunded {amount :: Coin c, tableId :: Text}
  | TableFundingFailed {failureReason :: Text}

deriving instance IsChain c => Eq (Result c)
deriving instance IsChain c => Show (Result c)

data Client c m = Client
  { newTable :: [Text] -> m (Result c)
  , fundTable :: Text -> Integer -> m (Result c)
  , notify :: m (Maybe (Result c))
  }

startClient ::
  forall c m.
  (IsChain c, Monad m, MonadDelay m, MonadThrow m) =>
  Server c m ->
  m (Client c m)
startClient server = pure $ Client{newTable, fundTable, notify}
 where
  newTable ps = do
    result <- initHead server ps
    let loop =
          result >>= \case
            InitDone{headId, parties} -> pure $ TableCreated{parties, tableId = headId}
            InitPending -> threadDelay 1 >> loop
            InitFailed{reason} -> pure $ TableCreationFailed{failureReason = reason}
    loop

  fundTable tid fund = do
    result <- commit server fund tid
    result >>= \case
      CommitDone c -> pure $ TableFunded c tid
      other -> pure $ TableFundingFailed $ asText other

  notify = pure Nothing

asText :: IsChain c => CommitResult c -> Text
asText CommitDone{coin} = pack $ "commit done with coin " <> show coin
asText NoMatchingCoin{value} = pack $ "no matching coins for value " <> show value

runClient :: HasIO m => Client c m -> m ()
runClient client = loop
 where
  loop = do
    prompt
    input >>= \case
      Left EOF -> pure ()
      Left (Err _) -> loop
      Right Quit -> output Bye
      Right cmd -> handleCommand client cmd >>= output >> loop

handleCommand :: HasIO m => Client c m -> Command -> m Output
handleCommand _client = error "not implemented"
