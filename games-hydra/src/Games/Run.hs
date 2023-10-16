{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Games.Run where

import Game.Client (runClient)
import Game.Client.Console (mkImpureIO)
import Game.Server.Hydra (withHydraServer)
import Games.Run.Cardano (withCardanoNode)
import Games.Run.Hydra (HydraNode (..), withHydraNode)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Game.BlackJack (BlackJack)

run :: IO ()
run = do
  hSetBuffering stdout NoBuffering
  withCardanoNode $ \cardano ->
    withHydraNode cardano $ \HydraNode{hydraHost} ->
      withHydraServer hydraHost $ \server -> do
        putStrLn "Starting client"
        runClient @BlackJack @_ @_ server mkImpureIO
