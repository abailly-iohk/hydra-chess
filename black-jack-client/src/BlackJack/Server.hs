module BlackJack.Server where

import Control.Exception (Exception)

data Message = Ping
    deriving (Eq, Show)

data ServerException = ServerException
    deriving (Eq, Show)

instance Exception ServerException

newtype Server m = Server
    { -- | Initialises connection to the server.
      -- Might throw a `ServerException`.
      connect :: m ()
    }

-- | Handle to interface for inbound messages.
type Callback m = Message -> m ()

-- | A type tying both inbound and outbound messages sending in a single /Component/.
type BlackJackServer m a = Callback m -> (Server m -> m a) -> m a
