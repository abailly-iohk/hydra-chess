module Main where

import BlackJack.Client (runClient, startClient)
import BlackJack.Client.Console ()
import BlackJack.Server.Mock (withMockServer)
import Control.Monad ((>=>))
import System.Environment (getArgs)

main :: IO ()
main = do
  [myId] <- getArgs
  withMockServer myId $ startClient >=> runClient
