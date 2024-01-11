module Games.Options where

import Games.Run.Cardano (Network)
import Options.Applicative (
  Parser,
  ParserInfo,
  auto,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  progDesc,
  short,
  switch,
  (<**>),
 )

data Options = Options
  { cardanoNetwork :: Network
  , onlyCardano :: Bool
  }
  deriving (Eq, Show)

cardanoNetworkParser :: Parser Network
cardanoNetworkParser =
  option
    auto
    ( long "network"
        <> short 'n'
        <> metavar "STRING"
        <> help "The well-known network to run game on, one of: Preview, Preprod, Mainnet"
    )

onlyCardanoParser :: Parser Bool
onlyCardanoParser =
  switch
    ( long "only-cardano"
        <> help "Only run cardano-node, does not start game or hydra servers (for testing purpose)"
    )

optionsParser :: Parser Options
optionsParser =
  Options <$> cardanoNetworkParser <*> onlyCardanoParser

hydraGamesInfo :: ParserInfo Options
hydraGamesInfo =
  info
    (optionsParser <**> helper)
    ( progDesc "hydra-games - start a Hydra game"
    )
