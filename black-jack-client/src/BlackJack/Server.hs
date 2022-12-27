module BlackJack.Server where

import Control.Exception (Exception)

data Message = Ping
  deriving (Eq, Show)

data ServerException = ServerException
  deriving (Eq, Show)

instance Exception ServerException

class IsParty p where
  partyId :: p -> String

data Server p m = Server
  { -- | Connects to given party.
    -- Might throw a `ServerException`.
    connect :: String -> m p
  , -- | Initialises a head with given parties.
    -- Those parties must have been connected to first.
    -- Might throw a `ServerException`.
    initHead :: [p] -> m ()
  }
