{-# LANGUAGE NamedFieldPuns #-}

module BlackJack.Client where

import BlackJack.Server (Server (..))

startClient :: Server m -> m ()
startClient Server{connect} = connect
