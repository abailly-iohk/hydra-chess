module BlackJack.Server where

data Message = Ping
    deriving (Eq, Show)

newtype Server m = Server
    { -- | Send a `Message` to the network
      broadcast :: Message -> m ()
    }

-- | Handle to interface for inbound messages.
type Callback m = Message -> m ()

-- | A type tying both inbound and outbound messages sending in a single /Component/.
type BlackJackServer m a = Callback m -> (Server m -> m a) -> m a
