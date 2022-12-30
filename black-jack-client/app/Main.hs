{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import BlackJack.Client (runClient, startClient)
import BlackJack.Client.Console (mkImpureIO)
import BlackJack.Server (Host (..))
import BlackJack.Server.Mock (MockParty (..), pid, withMockServer)
import Control.Monad ((>=>))
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

data Options = Options {myId :: String, myHost :: Host}
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
                  <> help "name or ip address to bind to (default: 127.0.0.1)."
              )
        )
    <*> option
      auto
      ( long "port"
          <> short 'p'
          <> metavar "PORT"
          <> value 56789
          <> help "port to listen on (default: 56789)."
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
  Options{myId, myHost} <- execParser blackJackClientInfo
  withMockServer Party{pid = pack myId, host = myHost} $ flip runClient mkImpureIO
