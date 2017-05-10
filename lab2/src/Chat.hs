-- | CS240h Lab 2 Chat Server
module Chat (chat) where

import qualified Data.ByteString as B
import Network.Connection
import Data.Default


-- | Chat server entry point.
chat :: Int -> IO ()
chat port = return ()

main = do
    ctx <- initConnectionContext
    con <- connectTo ctx $ ConnectionParams
                              { connectionHostname  = "www.example.com"
                              , connectionPort      = 4567
                              , connectionUseSecure = Nothing
                              , connectionUseSocks  = Nothing
                              }
    connectionPut con (B.singleton 0xa)
    r <- connectionGet con 1
    putStrLn $ show r
    connectionClose con

