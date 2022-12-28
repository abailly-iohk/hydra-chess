{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module BlackJack.Server where

import Control.Exception (Exception)
import Data.Text (Text)

data Message = Ping
  deriving (Eq, Show)

data ServerException = ServerException {reason :: Text}
  deriving (Eq, Show)

instance Exception ServerException

class IsParty p where
  partyId :: p -> Text

data Server p m = Server
  { -- | Connects to given party.
    -- Might throw a `ServerException`.
    connect :: Text -> m p
  , -- | Initialises a head with given parties.
    -- Those parties must have been connected to first.
    -- Returns an action that can be used to check whether or not the initialisation is
    -- done.
    -- Might throw a `ServerException`.
    initHead :: [p] -> m (m InitResult)
  }

data InitResult
  = InitDone {headId :: Text}
  | InitPending
  | InitFailed {reason :: Text}
  deriving stock (Eq, Show)
