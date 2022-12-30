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

import BlackJack.Client.IO (Command (..), Err (..), HasIO (..), Output (Bye, Ko, Ok))
import BlackJack.Server (HeadId (HeadId), Indexed (..), IsChain (..), Server (..))
import qualified BlackJack.Server as Server
import Control.Monad (forM_, unless)
import Control.Monad.Class.MonadAsync (MonadAsync, race_)
import Control.Monad.Class.MonadTimer (MonadDelay, threadDelay)
import Data.Functor ((<&>))
import Data.Text (Text, pack)

data Result c
  = TableCreated {parties :: [Party c], tableId :: Text}
  | TableCreationFailed {failureReason :: Text}
  | TableFunded {amount :: Coin c, tableId :: Text}
  | TableFundingFailed {failureReason :: Text}

deriving instance IsChain c => Eq (Result c)
deriving instance IsChain c => Show (Result c)

runClient :: (IsChain c, MonadAsync m, MonadDelay m) => Server c m -> HasIO m -> m ()
runClient server io = race_ loop (notify 0)
 where
  notify fromIndex = do
    Indexed{lastIndex, events} <- poll server fromIndex (fromIndex + 10)
    unless (null events) $ forM_ events (output io . Ok . pack . show)
    let newIndex = fromIndex + fromIntegral (length events)
    unless (lastIndex > newIndex) $ threadDelay 1
    notify newIndex

  loop = do
    prompt io
    input io >>= \case
      Left EOF -> pure ()
      Left (Err err) -> output io (Ko err) >> loop
      Right Quit -> output io Bye >> pure ()
      Right cmd -> handleCommand server cmd >>= output io >> loop

handleCommand :: (IsChain c, Monad m) => Server c m -> Command -> m Output
handleCommand Server{initHead, commit} = \case
  NewTable peers ->
    initHead peers <&> (\HeadId{headId} -> Ok . ("head initialised with id " <>) $ headId)
  FundTable tableId amount ->
    commit amount (HeadId tableId) >> pure (Ok "committed")
  Quit -> pure Bye
