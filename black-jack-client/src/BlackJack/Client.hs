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
import BlackJack.Server (FromChain (..), HeadId (HeadId), IsChain (..), Server (..))
import qualified BlackJack.Server as Server
import Control.Monad (forM_)
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

data Client c m = Client
  { newTable :: [Text] -> m HeadId
  , fundTable :: HeadId -> Integer -> m ()
  , notify :: m [Result c]
  }

startClient ::
  forall c m.
  (IsChain c, Monad m, MonadDelay m) =>
  Server c m ->
  m (Client c m)
startClient server = pure $ Client{newTable, fundTable, notify}
 where
  newTable ps = initHead server ps

  fundTable tid fund = commit server fund tid

  notify =
    poll server >>= \case
      HeadCreated{headId, parties} : _ -> pure [TableCreated{tableId = headId, parties}]
      [] -> pure []
      _ -> error "not implemented"

runClient :: (IsChain c, MonadAsync m, MonadDelay m) => Server c m -> HasIO m -> m ()
runClient server io = race_ loop notify
 where
  notify = do
    es <- poll server
    if null es
      then threadDelay 1
      else forM_ es (output io . Ok . pack . show)

  loop = do
    prompt io
    input io >>= \case
      Left EOF -> pure ()
      Left (Err err) -> output io (Ko err) >> loop
      Right Quit -> output io Bye >> pure ()
      Right cmd -> handleCommand server cmd >>= output io >> loop

handleCommand :: (IsChain c, Monad m) => Server c m -> Command -> m Output
handleCommand Server{initHead} = \case
  NewTable peers ->
    initHead peers <&> (\HeadId{headId} -> Ok . ("head initialised with id " <>) $ headId)
  Quit -> pure Bye
