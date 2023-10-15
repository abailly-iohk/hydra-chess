{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Games.Run where

import Control.Monad (forever)
import Control.Monad.Class.MonadTimer (threadDelay)
import Games.Run.Cardano (withCardanoNode)
import System.IO (BufferMode (..), hSetBuffering, stdout)

run :: IO ()
run = do
  hSetBuffering stdout NoBuffering
  runGame
 where
  runGame :: IO ()
  runGame =
    withCardanoNode $ \cardano ->
      -- hydra <- withHydraNode cardano
      -- server <- ContT $ withHydraServer (hydraHost hydra)
      -- lift $ runClient @Chess @_ @_ server mkImpureIO
      forever $ do
        putStrLn $ "running " <> show cardano
        threadDelay 1_000_000
