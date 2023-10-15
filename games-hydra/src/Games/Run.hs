{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Games.Run where

import Control.Monad.Cont (ContT (..), lift)
import Game.Chess (Chess)
import Game.Client (runClient)
import Game.Client.Console (mkImpureIO)
import Game.Server (Host)
import Game.Server.Hydra (withHydraServer)

data HydraNode = HydraNode {hydraHost :: Host}

data CardanoNode = CardanoNode {nodeSocket :: FilePath}

run :: IO ()
run = runContT runGame (const $ pure ())
 where
  runGame :: ContT () IO ()
  runGame = do
    cardano <- withCardanoNode
    hydra <- withHydraNode cardano
    server <- ContT $ withHydraServer (hydraHost hydra)
    lift $ runClient @Chess @_ @_ server mkImpureIO

  withHydraNode :: CardanoNode -> ContT () IO HydraNode
  withHydraNode = undefined

  withCardanoNode :: ContT () IO CardanoNode
  withCardanoNode = undefined
