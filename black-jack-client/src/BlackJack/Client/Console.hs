module BlackJack.Client.Console where

import BlackJack.Client.IO (Command)
import Data.Bifunctor (first)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parse)

type Parser = Parsec Void Text

readInput :: Text -> Either Text Command
readInput = first (pack . show) . parse inputParser ""

inputParser :: Parser Command
inputParser = error "not implemented"
