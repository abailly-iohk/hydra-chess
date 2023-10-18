{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Games.Run where

import Game.Client (runClient)
import Game.Client.Console (mkImpureIO)
import Games.Server.Hydra (withHydraServer, HydraParty (..))
import Games.Run.Cardano (withCardanoNode)
import Games.Run.Hydra (HydraNode (..), withHydraNode)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Game.Chess (Chess)
import Cardano.Binary (serialize')

run :: IO ()
run = do
  hSetBuffering stdout NoBuffering
  withCardanoNode $ \cardano ->
    withHydraNode cardano $ \HydraNode{hydraParty, hydraHost} -> do
      let party = HydraParty $ serialize' hydraParty
      withHydraServer party hydraHost $ \server -> do
        putStrLn $ "Starting client for " <> show party <> " and host " <> show hydraHost
        runClient @Chess @_ @_ server mkImpureIO
