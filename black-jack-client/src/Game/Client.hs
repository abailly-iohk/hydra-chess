{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Game.Client where

import Control.Monad (forM_, unless)
import Control.Monad.Class.MonadAsync (MonadAsync, race_)
import Control.Monad.Class.MonadTimer (MonadDelay, threadDelay)
import Data.Functor (void, (<&>))
import Data.Text (Text, pack)
import Game.Client.IO (Command (..), Err (..), HasIO (..), Output (Bye, Ko, Ok))
import Game.Server (Game, HeadId (HeadId), Indexed (..), IsChain (..), Server (..))
import qualified Game.Server as Server

data Result c
  = TableCreated {parties :: [Party c], tableId :: Text}
  | TableCreationFailed {failureReason :: Text}
  | TableFunded {amount :: Coin c, tableId :: Text}
  | TableFundingFailed {failureReason :: Text}

deriving instance IsChain c => Eq (Result c)
deriving instance IsChain c => Show (Result c)

runClient :: (Game g, IsChain c, MonadAsync m, MonadDelay m) => Server g c m -> HasIO m -> m ()
runClient server io = race_ loop (notify 0)
 where
  notify fromIndex = do
    Indexed{lastIndex, events} <- poll server fromIndex (fromIndex + 10)
    unless (null events) $ forM_ events (output io . Ok . pack . show)
    let newIndex = fromIndex + fromIntegral (length events)
    unless (lastIndex > newIndex) $ threadDelay 2000000
    notify newIndex

  loop = do
    prompt io
    input io >>= \case
      Left EOF -> pure ()
      Left (Err err) -> output io (Ko err) >> loop
      Right Quit -> void (output io Bye)
      Right cmd -> handleCommand server cmd >>= output io >> loop

handleCommand :: (Game g, IsChain c, Monad m) => Server g c m -> Command -> m Output
handleCommand Server{initHead, commit, play, closeHead, newGame} = \case
  NewTable peers ->
    initHead peers <&> (\HeadId{headId} -> Ok . ("head initialised with id " <>) $ headId)
  FundTable tableId amount ->
    commit amount (HeadId tableId) >> pure (Ok "committed")
  Play tableId p ->
    play (HeadId tableId) p >> pure (Ok "played")
  NewGame tableId ->
    newGame (HeadId tableId) >> pure (Ok "new game")
  Stop tableId ->
    closeHead (HeadId tableId) >> pure (Ok "closed")
  Quit -> pure Bye
