{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE LambdaCase #-}

module Game.Client.Console where

import Game.Client.IO (Command (..), Err (..), HasIO (..))
import Control.Applicative ((<|>))
import Control.Exception (IOException, handle)
import Data.Bifunctor (first)
import Data.Functor (($>))
import Data.Text (Text, pack)
import qualified Data.Text.IO as Text
import Data.Void (Void)
import System.IO.Error (isEOFError)
import Text.Megaparsec (Parsec, empty, many, parse, sepBy, try, takeRest)
import Text.Megaparsec.Char (alphaNumChar, space, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Aeson (Value, eitherDecode)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy as LBS

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
  Play <$> (identifier <* spaceConsumer) <*> parsePlay
 where
   parsePlay :: Parser Value
   parsePlay = takeRest >>= (\case
     Left err -> fail err
     Right v -> pure v) . eitherDecode . LBS.fromStrict . encodeUtf8


newGameParser :: Parser Command
newGameParser = do
  string "newGame" >> spaceConsumer
  NewGame <$> (identifier <* spaceConsumer)

stopParser :: Parser Command
stopParser = do
  string "stop" >> spaceConsumer
  Stop <$> (identifier <* spaceConsumer)

identifier :: Parser Text
identifier = pack <$> ((:) <$> alphaNumChar <*> many alphaNumChar)

spaceConsumer :: Parser ()
spaceConsumer =
  L.space space1 (L.skipLineComment "#") empty
