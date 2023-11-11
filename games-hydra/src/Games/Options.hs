module Games.Options where

import Games.Run.Cardano (Network)
import Options.Applicative (Parser, ParserInfo, auto, help, helper, info, long, metavar, option, progDesc, short, (<**>))

data Options = Options {cardanoNetwork :: Network}
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

optionsParser :: Parser Options
optionsParser =
  Options <$> cardanoNetworkParser

hydraGamesInfo :: ParserInfo Options
hydraGamesInfo =
  info
    (optionsParser <**> helper)
    ( progDesc "hydra-games - start a Hydra game"
    )
