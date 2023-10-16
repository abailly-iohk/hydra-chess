{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Games.Run where

import Control.Monad (forever)
import Control.Monad.Class.MonadTimer (threadDelay)
import Games.Run.Cardano (withCardanoNode)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Games.Run.Hydra (withHydraNode)

run :: IO ()
run = do
  hSetBuffering stdout NoBuffering
  runGame
 where
  runGame :: IO ()
  runGame =
    withCardanoNode $ \cardano ->
      withHydraNode cardano $ \hydra ->
        -- server <- ContT $ withHydraServer (hydraHost hydra)
        -- lift $ runClient @Chess @_ @_ server mkImpureIO
        forever $ do
          putStrLn $ "running " <> show hydra
          threadDelay 1_000_000
