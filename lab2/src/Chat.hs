-- | CS240h Lab 2 Chat Server
-- Pretty much a carbon copy of: https://wiki.haskell.org/Implement_a_chat_server
module Chat (chat) where

import           Control.Concurrent
import           Control.Exception
import           Network.Socket     hiding (recv, recvFrom, send, sendTo)
import           System.IO
-- import Control.Concurrent.Chan
import           Control.Monad.Fix  (fix)
import           Control.Monad (unless)

chat :: Int -> IO ()
chat port = do
  -- Set up a socket to receive connections on
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet (fromIntegral port) iNADDR_ANY)
  listen sock 2 -- Maximum number of queued (non-accepted) connections
  -- Communication channel between connection threads
  chan <- newChan
  mainLoop sock chan 1


type Msg = (Int, String)


-- | Continuously accept connections on socket @sock@,
-- fork them to a new thread and let the threads communicate
-- on @chan@
mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan n = do
  conn <- accept sock
  _ <- forkIO (runConn conn chan n)
  mainLoop sock chan (n+1)


-- | Relay messages from this connection to all others in a loop
runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan nameI = do
    let name = show nameI
        broadcast msg = writeChan chan (nameI, msg) -- send msg to all connections
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    broadcast (name ++ " has joined")
    -- hPutStrLn hdl ("Welcome, " ++ name ++ "!")

    commLine <- dupChan chan

    -- Fork off a thread for reading and printing messages from the duplicated channel
    reader <- forkIO $ fix $ \loop -> do
        (nameI', msg) <- readChan commLine
        unless (nameI == nameI') $ hPutStrLn hdl msg
        loop

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- fmap init (hGetLine hdl)
        case line of
             -- If an exception is caught, send a message and break the loop
             "quit" -> hPutStrLn hdl "Bye!"
             -- else, continue looping.
             _      -> broadcast (name ++ ": " ++ line) >> loop

    killThread reader                      -- kill after the loop ends
    broadcast (name ++ " has left")        -- make a final broadcast
    hClose hdl                             -- close the handle
