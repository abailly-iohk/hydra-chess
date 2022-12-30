{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module BlackJack.Client.IO where

import Control.Monad.State (MonadState, State, StateT, execState, gets, modify, runState, runStateT)
import Control.Monad.Trans (MonadTrans)
import Data.Bifunctor (second)
import Data.Text (Text, pack)
import Data.Void (Void)
import Prelude hiding (getLine, print)

data Command
  = NewTable [Text]
  | FundTable Text Integer
  | Quit
  deriving (Eq, Show)

data Output
  = Bye
  | Ok Text
  | Ko Text
  deriving (Eq, Show)

data Err = EOF | Err Text
  deriving (Eq, Show)

data HasIO m = HasIO
  { output :: Output -> m ()
  , input :: m (Either Err Command)
  , prompt :: m ()
  }

-- * Pure IO

data PureIO = PureIO
  { inputText :: [Command]
  , outputText :: [Output]
  }

mkPureIO :: (MonadState PureIO m) => HasIO m
mkPureIO =
  HasIO
    { input = do
        ins <- gets inputText
        case ins of
          [] -> pure $ Left EOF
          (t : ts) -> modify (\e -> e{inputText = ts}) >> pure (Right t)
    , output = \t -> modify $ \e -> e{outputText = t : outputText e}
    , prompt = pure ()
    }

withInput ::
  Monad m =>
  [Command] ->
  StateT PureIO m a ->
  m (a, [Output])
withInput stream act =
  second outputText <$> runStateT act (PureIO stream [])
