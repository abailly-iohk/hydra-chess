{-# LANGUAGE DerivingStrategies #-}

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
    -- Returns an action that can be used to check whether or not the initialisation is
    -- done.
    -- Might throw a `ServerException`.
    initHead :: [p] -> m (m InitResult)
  }

data InitResult = InitDone | InitPending | InitFailed String
  deriving stock (Eq, Show)
