{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chess.Parse where

import Chess.Game (Move (..), Position (..))
import Control.Monad.Catch (Exception)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.State (MonadState, evalStateT, get, put)
import Data.Char (ord)
import Data.Functor (void)
import Data.Text (Text, pack)

newtype Err = Err Text
  deriving (Eq, Show)

instance Exception Err

type ParseResult = Either Err Move

parseMove :: String -> ParseResult
parseMove = evalStateT parser

parser :: (MonadError Err m, MonadState String m) => m Move
parser = do
  c <- parseColumn
  r <- parseRow
  dash
  c' <- parseColumn
  r' <- parseRow
  pure $ Move (Pos r c) (Pos r' c')
 where
  parseError = throwError . Err . pack

  dash = do
    s <- get
    case s of
      ('-' : rest) -> void (put rest)
      _ -> parseError $ "expected '-', found: " <> s

  parseColumn =
    fromIntegral <$> do
      s <- get
      case s of
        (c : rest) ->
          let r = ord c - ord 'a'
           in if r >= 0 && r <= 7
                then put rest >> pure r
                else parseError $ "unknown column " <> show c
        [] ->
          throwError $ Err "no more characters, expected a column identifier (a-h)"

  parseRow =
    fromIntegral <$> do
      s <- get
      case s of
        (c : rest) ->
          let r = ord c - ord '1'
           in if r >= 0 && r <= 7
                then put rest >> pure r
                else parseError $ "unknown row " <> show c
        [] ->
          throwError $ Err "no more characters, expected a row  identifier (1-8)"
