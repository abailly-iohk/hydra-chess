{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BlackJack.Server.Hydra where
import BlackJack.Server (Host (..), Server (..), HeadId, Indexed)
import qualified Network.WebSockets as WS
import Control.Concurrent.Class.MonadSTM (newTQueueIO, TQueue, atomically, readTQueue, writeTQueue)
import Control.Monad.Class.MonadAsync (withAsync, concurrently_)
import Network.WebSockets (Connection, runClient)
import Control.Exception (IOException, catch)
import Data.Text (unpack, Text)
import Control.Monad (forever)

-- | The type of backend provide by Hydra
data Hydra

withHydraServer :: Host -> (Server Hydra IO -> IO a) -> IO a
withHydraServer host k = do
  fromq <- newTQueueIO
  toq <- newTQueueIO
  withAsync (wsClient fromq toq) $ \ _ ->
    let server = Server
                  {initHead = sendInit (toq, fromq),
                   commit = sendCommit (toq, fromq),
                   poll = pollEvents (toq, fromq),
                   play = playGame (toq, fromq),
                   newGame = restartGame (toq, fromq),
                   closeHead = sendClose (toq, fromq)}
    in k server

 where
  wsClient :: TQueue IO WS.Message -> TQueue IO WS.Message -> IO ()
  wsClient fromq toq =
    withClient host $ \cnx ->
      concurrently_
        (forever $ atomically (readTQueue toq) >>= WS.send cnx)
        (forever $ WS.receive cnx >>= atomically . writeTQueue fromq)

  sendInit :: (TQueue IO WS.Message, TQueue IO WS.Message) -> [Text] -> IO HeadId
  sendInit = error "not implemented"

  sendClose :: (TQueue IO WS.Message, TQueue IO WS.Message) -> HeadId -> IO ()
  sendClose = error "not implemented"

  restartGame :: (TQueue IO WS.Message, TQueue IO WS.Message) -> HeadId -> IO ()
  restartGame = error "not implemented"

  playGame :: (TQueue IO WS.Message, TQueue IO WS.Message) -> HeadId -> Int -> IO ()
  playGame = error "not implemented"

  pollEvents :: (TQueue IO WS.Message, TQueue IO WS.Message) -> Integer -> Integer -> IO (Indexed c)
  pollEvents = error "not implemented"

  sendCommit :: (TQueue IO WS.Message, TQueue IO WS.Message) -> Integer -> HeadId -> IO ()
  sendCommit = error "not implemented"


withClient :: Host -> (Connection -> IO ()) -> IO ()
withClient Host{host, port} action = retry
 where
  retry = runClient (unpack host) (fromIntegral port) "/" action
    `catch` \(_ :: IOException) -> retry
