{-# LANGUAGE NamedFieldPuns #-}

import HttpServer (httpServer)
import Options.Applicative (
  Parser,
  ParserInfo,
  auto,
  execParser,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  progDesc,
  short,
  strOption,
  value,
  (<**>),
 )

data Options = Options {host :: String, port :: Int}
  deriving (Eq, Show)

hostParser :: Parser String
hostParser =
  strOption
    ( long "host"
        <> short 'h'
        <> metavar "HOST"
        <> value "127.0.0.1"
        <> help "name or ip address to bind to (default: 127.0.0.1)."
    )

portParser :: Parser Int
portParser =
  option
    auto
    ( long "port"
        <> short 'p'
        <> metavar "PORT"
        <> value 56789
        <> help "port to listen on (default: 56789)."
    )

optionsParser :: Parser Options
optionsParser =
  Options <$> hostParser <*> portParser

mockServerOptionsInfo :: ParserInfo Options
mockServerOptionsInfo =
  info
    (optionsParser <**> helper)
    ( progDesc "mock-chain - A dumb backend for black-jack clients"
    )

main :: IO ()
main = do
  Options{host, port} <- execParser mockServerOptionsInfo
  httpServer host port
