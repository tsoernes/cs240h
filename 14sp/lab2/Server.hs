module Server (main) where

import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Chat (chat)

defaultPort :: Int
defaultPort = 8181

-- | Run our chat server.
main :: IO ()
main = do
  port <- getPort
  putStrLn $ "Using port: " ++ show port
  chat port

getPort :: IO Int
getPort = do
  port <- lookupEnv "CHAT_SERVER_PORT"
  let intPort = fromMaybe defaultPort (port >>= read)
  return intPort

