{-# LANGUAGE OverloadedStrings #-}

module BlackJack.Client.Console where

import BlackJack.Client.IO (Command (Quit))
import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.Functor (($>))
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parse, try)
import Text.Megaparsec.Char (string)

type Parser = Parsec Void Text

readInput :: Text -> Either Text Command
readInput = first (pack . show) . parse inputParser ""

inputParser :: Parser Command
inputParser = (try (string "q") <|> string "quit") $> Quit
