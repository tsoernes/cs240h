-- | Runner for Chat server. We put Main separately so that we can keep chat as
-- a library for testing.
module Main (main) where

import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Chat

defaultPort :: Int
defaultPort = 8181

-- | Run our chat server.
main :: IO ()
main = do
  port <- getPort
  chat port

getPort :: IO Int
getPort = do
  port <- lookupEnv "CHAT_SERVER_PORT"
  let intPort = fromMaybe defaultPort (port >>= readMaybe)
  return intPort

readMaybe :: Read a => String -> Maybe a
readMaybe str = case reads str of
                  [(val, "")] -> Just val
                  _ -> Nothing
