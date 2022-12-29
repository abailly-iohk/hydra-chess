{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module BlackJack.Client.IO where

import Control.Monad.State (MonadState, State, execState, gets, modify, runState)
import Data.Bifunctor (second)
import Data.Text (Text, pack)
import Data.Void (Void)
import Prelude hiding (getLine, print)

data Command
  = NewTable [Text]
  | Quit
  deriving (Eq, Show)

data Output = Bye
  deriving (Eq, Show)

data Err = EOF | Err Text
  deriving (Eq, Show)

class Monad m => HasIO m where
  output :: Output -> m ()
  input :: m (Either Err Command)
  prompt :: m ()

-- * Pure IO

data PureIO = PureIO
  { inputText :: [Command]
  , outputText :: [Output]
  }

newtype Pure a = Pure {runPure :: State PureIO a}
  deriving (Functor, Applicative, Monad, MonadState PureIO)

instance HasIO Pure where
  input = do
    ins <- gets inputText
    case ins of
      [] -> pure $ Left EOF
      (t : ts) -> modify (\e -> e{inputText = ts}) >> pure (Right t)

  output t = modify $ \e -> e{outputText = t : outputText e}
  prompt = pure ()

withInput ::
  [Command] -> Pure a -> (a, [Output])
withInput stream act =
  second outputText $ runState (runPure act) (PureIO stream [])
