module BlackJack.ClientSpec where

import BlackJack.Client (startClient)
import BlackJack.Server (Server (..), ServerException (ServerException))
import Control.Exception (throwIO)
import Test.Hspec (Spec, anyException, around, it, shouldThrow)

spec :: Spec
spec = around failingServer $
    it "throws exception when failing to connect to server" $ \server ->
        startClient server `shouldThrow` anyException

failingServer :: (Server IO -> IO ()) -> IO ()
failingServer act =
    act Server{connect = throwIO ServerException}
