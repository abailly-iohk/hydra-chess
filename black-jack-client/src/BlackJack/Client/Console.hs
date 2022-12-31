{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module BlackJack.Client.Console where

import BlackJack.Client.IO (Command (..), Err (..), HasIO (..))
import Control.Applicative ((<|>))
import Control.Exception (IOException, handle)
import Data.Bifunctor (first)
import Data.Functor (($>))
import Data.Text (Text, pack)
import qualified Data.Text.IO as Text
import Data.Void (Void)
import System.IO.Error (isEOFError)
import Text.Megaparsec (Parsec, empty, many, parse, sepBy, try)
import Text.Megaparsec.Char (alphaNumChar, space, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

mkImpureIO :: HasIO IO
mkImpureIO =
  HasIO
    { input = handle eofException $ do
        inp <- readInput <$> Text.getLine
        case inp of
          Left err -> pure $ Left $ Err err
          Right cmd -> pure $ Right cmd
    , output = print
    , prompt = putStr "> "
    }

eofException :: IOException -> IO (Either Err Command)
eofException e
  | isEOFError e = pure (Left EOF)
  | otherwise = pure (Left $ Err $ pack $ show e)

readInput :: Text -> Either Text Command
readInput = first (pack . show) . parse inputParser ""

inputParser :: Parser Command
inputParser =
  quitParser <|> newTableParser
    <|> fundTableParser
    <|> playParser
    <|> newGameParser
    <|> stopParser

quitParser :: Parser Command
quitParser = (try (string "q") <|> string "quit") $> Quit

newTableParser :: Parser Command
newTableParser = do
  string "newTable" >> spaceConsumer
  NewTable <$> sepBy identifier space

fundTableParser :: Parser Command
fundTableParser = do
  string "fundTable" >> spaceConsumer
  FundTable <$> (identifier <* spaceConsumer) <*> L.decimal

playParser :: Parser Command
playParser = do
  string "play" >> spaceConsumer
  Play <$> (identifier <* spaceConsumer) <*> L.decimal

newGameParser :: Parser Command
newGameParser = do
  string "newGame" >> spaceConsumer
  NewGame <$> (identifier <* spaceConsumer)

stopParser :: Parser Command
stopParser = do
  string "stop" >> spaceConsumer
  NewGame <$> (identifier <* spaceConsumer)

identifier :: Parser Text
identifier = pack <$> ((:) <$> alphaNumChar <*> many alphaNumChar)

spaceConsumer :: Parser ()
spaceConsumer =
  L.space space1 (L.skipLineComment "#") empty
