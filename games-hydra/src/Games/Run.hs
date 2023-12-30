{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module Games.Run where

import Cardano.Binary (serialize')
import Control.Monad (forever)
import Control.Monad.Class.MonadTimer (threadDelay)
import Game.Chess (Chess)
import Game.Client (runClient)
import Game.Client.Console (mkImpureIO)
import Games.Options (Options (..), hydraGamesInfo)
import Games.Run.Cardano (CardanoNode (..), withCardanoNode)
import Games.Run.Hydra (HydraNode (..), withHydraNode)
import Games.Server.Hydra (HydraParty (..), withHydraServer)
import Options.Applicative (execParser)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Game.Client.Console (inputParser)
import Games.Server.IO (notifyChessEvent)

run :: IO ()
run = do
  Options{cardanoNetwork, onlyCardano} <- execParser hydraGamesInfo
  hSetBuffering stdout NoBuffering
  withCardanoNode cardanoNetwork $ \cardano ->
    if onlyCardano
      then runCardanoClient
      else startServers cardano
 where
  runCardanoClient =
    forever (threadDelay 60_000_000)

  startServers cardano@CardanoNode{network} =
    withHydraNode cardano $ \HydraNode{hydraParty, hydraHost} -> do
      let party = HydraParty $ serialize' hydraParty
      withHydraServer network party hydraHost $ \server -> do
        putStrLn $ "Starting client for " <> show party <> " and host " <> show hydraHost
        runClient @Chess @_ @_ server notifyChessEvent (mkImpureIO inputParser)
