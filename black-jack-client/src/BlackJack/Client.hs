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
import Control.Monad.Class.MonadThrow (MonadThrow)
import Control.Monad.Class.MonadTimer (MonadDelay)
import Data.Functor ((<&>))
import Data.Text (Text)

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
  (IsChain c, Monad m, MonadDelay m, MonadThrow m) =>
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

runClient :: (IsChain c, Monad m) => Client c m -> HasIO m -> m ()
runClient client io = loop
 where
  loop = do
    prompt io
    input io >>= \case
      Left EOF -> pure ()
      Left (Err err) -> output io (Ko err) >> loop
      Right Quit -> output io Bye >> pure ()
      Right cmd -> handleCommand client cmd >>= output io >> loop

handleCommand :: (IsChain c, Monad m) => Client c m -> Command -> m Output
handleCommand Client{newTable} = \case
  NewTable peers ->
    newTable peers <&> (\HeadId{headId} -> Ok . ("head initialised with id " <>) $ headId)
  Quit -> pure Bye
