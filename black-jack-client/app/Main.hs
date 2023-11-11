{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Text (Text, pack)
import Game.BlackJack (BlackJack)
import Game.Client (runClient)
import Game.Client.Console (mkImpureIO)
import Game.Server (Host (..))
import Game.Server.Mock (MockParty (..), withMockServer)
import Options.Applicative (
  Parser,
  ParserInfo,
  auto,
  command,
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
  subparser,
  value,
  (<**>),
  (<|>),
 )
import System.IO (BufferMode (NoBuffering), hSetBuffering, stderr, stdout)

data Options
  = HydraOptions {myId :: String, hydraServer :: Host}
  | MockOptions {pid :: Text, host :: Host}
  deriving (Eq, Show)

idParser :: Parser String
idParser =
  strOption
    ( long "host-id"
        <> short 'i'
        <> metavar "STRING"
        <> help "unique identifier for this node"
    )

hostParser :: Parser Host
hostParser =
  Host
    <$> ( pack
            <$> strOption
              ( long "host"
                  <> short 'H'
                  <> metavar "HOST"
                  <> value "127.0.0.1"
                  <> help "name or ip address of the server to connect to (default: 127.0.0.1)."
              )
        )
    <*> option
      auto
      ( long "port"
          <> short 'p'
          <> metavar "PORT"
          <> value 4001
          <> help "port of the server to connect to (default: 4001)."
      )

optionsParser :: Parser Options
optionsParser =
  hydraCommand <|> mockCommand
 where
  hydraCommand =
    subparser $
      command
        "hydra"
        ( info
            (helper <*> hydraOptionsParser)
            ( progDesc "Run a game client connecting to a Hydra instance"
            )
        )

  mockCommand =
    subparser $
      command
        "mock"
        ( info
            (helper <*> mockOptionsParser)
            ( progDesc "Run a game client connecting to a mock HTTP instance"
            )
        )

hydraOptionsParser :: Parser Options
hydraOptionsParser =
  HydraOptions <$> idParser <*> hostParser

mockOptionsParser :: Parser Options
mockOptionsParser =
  MockOptions
    <$> (pack <$> idParser)
    <*> hostParser

blackJackClientInfo :: ParserInfo Options
blackJackClientInfo =
  info
    (optionsParser <**> helper)
    ( progDesc "black-jack-client - A terminal-based interface to play Hydra Black Jack"
    )

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  execParser blackJackClientInfo >>= \case
    HydraOptions{} ->
      undefined -- withHydraServer hydraServer $ flip (runClient @BlackJack) mkImpureIO
    MockOptions{pid, host} ->
      withMockServer (Party host pid) $ flip (runClient @BlackJack) mkImpureIO
