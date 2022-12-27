module BlackJack.ClientSpec where

import BlackJack.Client (startClient)
import BlackJack.Server (Server (..), ServerException (ServerException))
import Control.Exception (throwIO)
import Test.Hspec (Spec, anyException, it, shouldThrow)

spec :: Spec
spec = do
  it "throws exception when failing to connect to server" $
    startClient unconnectedServer `shouldThrow` anyException

unconnectedServer :: Server IO
unconnectedServer = Server{connect = throwIO ServerException}
