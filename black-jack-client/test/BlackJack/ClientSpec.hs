module BlackJack.ClientSpec where

import BlackJack.Server (Server)
import Test.Hspec (ActionWith, Spec, anyException, around, it, shouldThrow)

spec :: Spec
spec = around failingServer $
    it "does not start when fail to connect to server" $ \server ->
        startClient server `shouldThrow` anyException

failingServer :: ActionWith (Server IO) -> IO ()
failingServer = error "not implemented"

startClient :: Server m -> m ()
startClient = error "not implemented"
