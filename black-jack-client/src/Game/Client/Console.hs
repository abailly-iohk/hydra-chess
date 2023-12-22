{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Game.Client.Console where

import Control.Applicative ((<|>))
import Control.Exception (IOException, handle)
import Data.Aeson (ToJSON (..), Value, eitherDecode, object, (.=))
import Data.Aeson.Key (fromText)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LBS
import Data.Functor (void, ($>))
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as Text
import Data.Void (Void)
import Game.Client.IO (Command (..), Err (..), HasIO (..))
import System.IO.Error (isEOFError)
import Text.Megaparsec (Parsec, empty, many, parse, sepBy, takeRest, try)
import Text.Megaparsec.Char (alphaNumChar, char, hexDigitChar, space, space1, string)
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
  quitParser
    <|> newTableParser
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
  parsePlay =
    takeRest
      >>= ( \case
              Left err -> fail err
              Right v -> pure v
          )
        . eitherDecode
        . LBS.fromStrict
        . encodeUtf8

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

hexString :: Parser Text
hexString = pack <$> ((:) <$> hexDigitChar <*> many hexDigitChar)

spaceConsumer :: Parser ()
spaceConsumer =
  L.space space1 (L.skipLineComment "#") empty

data FullTxOut = FullTxOut {txIn :: Text, address :: Text, value :: Coins}
  deriving stock (Eq, Show)

instance ToJSON FullTxOut where
  toJSON FullTxOut{txIn, address, value} =
    object
      [ fromText txIn
          .= object
            [ "address" .= address
            , "value" .= value
            ]
      ]

mkFullTxOut :: Text -> SimpleTxOut -> FullTxOut
mkFullTxOut address SimpleTxOut{txIn, coins} = FullTxOut{txIn, address, value = coins}

data SimpleTxOut = SimpleTxOut {txIn :: Text, coins :: Coins}
  deriving stock (Eq, Show)

data Coins = Coins
  { lovelace :: Integer
  , natives :: Map Text Coin
  }
  deriving stock (Eq, Show)

newtype Coin = Coin (Map Text Integer)
  deriving stock (Eq, Show)
  deriving newtype (ToJSON)

instance ToJSON SimpleTxOut where
  toJSON (SimpleTxOut txIn coins) = object [fromText txIn .= coins]

instance ToJSON Coins where
  toJSON Coins{lovelace, natives} =
    object $
      ["lovelace" .= lovelace]
        <> fmap
          ( \(pid, coin) ->
              fromText pid .= coin
          )
          (Map.toList natives)

-- | Parse a single line of a UTxO for TxIn and value parts.
-- TODO: Move this elsewhere, this has nothing to do here
parseQueryUTxO :: Text -> Either Text SimpleTxOut
parseQueryUTxO = first (pack . show) . parse utxoParser ""

utxoParser :: Parser SimpleTxOut
utxoParser = do
  txIn <- txInParser <* spaceConsumer
  value <- valueParser
  pure $ SimpleTxOut txIn value

valueParser :: Parser Coins
valueParser = do
  lovelace :: Integer <- L.decimal <* spaceConsumer <* string "lovelace" <* spaceConsumer
  pairs <- many tokenOrDatumParser
  pure $ Coins{lovelace, natives = Map.fromList $ makeTokens (catMaybes pairs)}

makeTokens :: [(Text, Text, Integer)] -> [(Text, Coin)]
makeTokens tokens = fmap mkObject $ List.groupBy samePid tokens
 where
  samePid (p, _, _) (p', _, _) = p == p'

mkObject :: [(Text, Text, Integer)] -> (Text, Coin)
mkObject tokens = (pid, Coin $ Map.fromList tokensAndValues)
 where
  (pid, _, _) = head tokens
  tokensAndValues = fmap (\(_, tok, val) -> (tok, val)) tokens

tokenOrDatumParser :: Parser (Maybe (Text, Text, Integer))
tokenOrDatumParser =
  (Just <$> try tokenParser) <|> (Nothing <$ datumParser)

datumParser :: Parser ()
datumParser = void $ string "+ TxOutDatumNone"

tokenParser :: Parser (Text, Text, Integer)
tokenParser = do
  void $ char '+' <* spaceConsumer
  amount :: Integer <- L.decimal <* spaceConsumer
  policyId <- hexString <* char '.'
  tokenName <- hexString <* spaceConsumer
  pure $ (policyId, tokenName, amount)

txInParser :: Parser Text
txInParser = do
  txId <- hexString <* spaceConsumer
  txIx :: Integer <- L.decimal
  pure $ txId <> "#" <> pack (show txIx)
