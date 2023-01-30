{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import BlackJack.Client (runClient)
import BlackJack.Client.Console (mkImpureIO)
import BlackJack.Server (Host (..))
import Data.Text (pack)
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
import System.IO (BufferMode (NoBuffering), hSetBuffering, stderr, stdout)
import BlackJack.Server.Hydra (withHydraServer)

data Options = Options {myId :: String, hydraServer :: Host}
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
                  <> short 'h'
                  <> metavar "HOST"
                  <> value "127.0.0.1"
                  <> help "name or ip address of the Hydra server to connect to (default: 127.0.0.1)."
              )
        )
    <*> option
      auto
      ( long "port"
          <> short 'p'
          <> metavar "PORT"
          <> value 4001
          <> help "port of Hydra server to connect to (default: 4001)."
      )

optionsParser :: Parser Options
optionsParser =
  Options <$> idParser <*> hostParser

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
  Options{hydraServer} <- execParser blackJackClientInfo
  withHydraServer hydraServer $ flip runClient mkImpureIO
