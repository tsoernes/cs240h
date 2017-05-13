{-# OPTIONS_GHC -Wno-unused-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- For writing and reading DB to file with Aeson
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Trahs where

import Control.Applicative
import System.Environment
import System.Exit
import System.Process
import System.IO
import Codec.Digest.SHA
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.Bifunctor (bimap)
import Control.Monad (when, unless)
import Data.Maybe (fromMaybe)
import System.Directory (getDirectoryContents, doesFileExist)
import System.Random (randomR, getStdGen, getStdRandom)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, encode, decodeStrict)

dbPath :: String
dbPath = ".trahs.db"

-- 1: Check args
-- 2: Connect to server
-- 3: Load and deserialize local db
trahs :: IO ()
trahs = do
  args <- getArgs
  case args of
    ["--server", l] -> do hSetBuffering stdout LineBuffering
                          server stdin stdout l
    [r, l] | (host, ':':rdir) <- break (== ':') r -> connect host rdir l
    _ -> do hPutStrLn stderr "usage: trahs HOST:DIR LOCALDIR"
            exitFailure

-- | Command for executing trahs on a remote system.  The '@' will be
-- replaced by the hostname, and the directory will be appended.
traSSH :: String
traSSH = "ssh -CTaxq @ ./trahs --server"

-- | @server r w dir@ runs the code to serve the contents of @dir@,
-- reading input from @r@ and writing it to @w@.
server :: Handle -> Handle -> FilePath -> IO ()
server r w dir = do
  hPutStrLn w "I am the server"
  line <- hGetLine r
  case line of
    -- Switch roles
    "TURN" -> client False r w dir
    -- Process command and keep looping
    _ -> exitFailure

-- | @client turn r w dir@ runs the client to update @dir@ based on
-- the remote contents, i.e. receive updates from remote server.
-- Commands for the remote server are written to @w@, while replies
-- are read from @r@.  If @turn@, then when done
-- the client should attempt to swap roles and run the protocol in the
-- other direction (uploading any changes to the other side).
-- Otherwise, if @turn@ is false, @client@ should simply return when
-- done.
client :: Bool -> Handle -> Handle -> FilePath -> IO ()
client turn r w dir = do
  line <- hGetLine r
  hPutStrLn stderr $ "The server said " ++ show line
  hPutStrLn w "Hello, server"
  line' <- hGetLine r
  hPutStrLn stderr $ "The server said " ++ show line'

  -- At the end, if `turn == True`, then we issue the TURN command to
  -- the server in order to swap roles and run `server r w dir` in
  -- order to upload changes to remote server.
  when turn $ do hPutStrLn w "TURN"
                 server r w dir

-- | Generate command for connecting to a remote host
-- with address @host@ through SSH and launching trash
-- on remote directory @dir@
hostCmd :: String -> FilePath -> IO String
hostCmd host dir = do
  tmpl <- fromMaybe traSSH <$> lookupEnv "TRASSH"
  case break (== '@') tmpl of
    (b, '@':e) -> return $ b ++ host ++ e ++ ' ':dir
    _          -> return $ tmpl ++ ' ':dir

-- | Spawn trahs process on remote host with the given
-- address @host@ and directory @dir@
spawnRemote :: String -> FilePath -> IO (Handle, Handle)
spawnRemote host dir = do
  cmd <- hostCmd host dir
  hPutStrLn stderr $ "running " ++ show cmd
  (Just w, Just r, _, _) <- createProcess (shell cmd) {
        std_in = CreatePipe
      , std_out = CreatePipe
    }
  hSetBuffering w LineBuffering
  return (r, w)

-- | Connect to @host@ address and sync remote
-- directory @rdir@ with local directory @ldir@.
-- Once finished, @ldir@ on the client and @rdir@
-- on server @host@ should have the same contents.
connect :: String -> FilePath -> FilePath -> IO ()
connect host rdir ldir = do
  (r, w) <- spawnRemote host rdir
  client True r w ldir

hashFile :: FilePath -> IO String
hashFile path = showBSasHex <$> (hash SHA256 <$> BSL.readFile path)


-- The first phase consists of finding modified files in the local directory.
-- trahs must read the directory and compare each file to the hash last recorded.
-- If the file has changed, trahs sets the file's write stamp to the local replica
-- ID and version number (as the changes are not reflected on any other replica).
updateDB :: FilePath -> IO ()
updateDB path = do
  files <- getDirectoryContents path >>= mapM readFile
  -- paths <- getDirectoryContents path
  -- files <- mapM readFile paths
  let hashes = map hashFile files
      -- Compare new hashed with
  return ()



-- In the second phase, the server simply sends the client its version vector and
-- a list of (file name, writestamp) pairs describing the contents of the directory.
-- (Besides a writestamp, the per-file information can be augmented with other
-- information such as SHA-256 hashes.
-- You may find it simplest just to dump the server's entire database to the client.)
sendDB :: ()
sendDB = undefined

-- In the third phase, the client merges the remote server state into its own local state.
mergeState :: VersionVector -- ^ Local version vec
           -> VersionVector -- ^ Remove verison vec
           -> [WriteStamp]  -- ^ Local writestamps
           -> [WriteStamp]  -- ^ Remote writestamps
           -> IO ()
mergeState lvv rvv lwss rwss = undefined
-- If the file exists on both the client and server and LWS = RWS, there is nothing to do.
-- If the files differ, but "version RWS ≤ LVV!replica(RWS)", then the client already learned about the server's version in some previous synchronization and subsequently overwrote it. Hence, the client ignores the server's version and keeps its own with no change.


-- A unique replica ID. The first time trahs is run on a particular directory,
-- it should randomly generate a replica ID for itself.
-- The replica ID should then never change.
type ReplicaID = Int

-- A local version number starts at 1 and increments every time trahs is run (and server is connectable).
type VersionNum = Int

-- In the fourth phase, the client sets its version vector to contain the
-- element-wise maximum of its previous contents and the values in the
-- remote server's version vector. This ensures that ∀R.LVV!R ≥ RVV!R,
-- reflecting the fact that the client now knows everything the server knows.
updateVersionVec :: VersionVector -- ^ Local (client) version vector
                 -> VersionVector -- ^ Remote (server) version vector
                 -> IO ()
updateVersionVec lvv rvv = undefined

-- A version vector is a map from replica ID to version number.
-- A replica's own replica ID always maps to its latest local version number.
-- Conceptually, any replica IDs not in the version vector are mapped to version number 0.
-- M.findWithDefault 0 key
--
-- After synchronizing from a remote replica, the local replica should set
-- its version vector to the element-wise maximum of old local version vector
-- and the remote version vector.
type VersionVector = M.Map ReplicaID VersionNum

type FileInfo = M.Map FilePath WriteStamp

data WriteStamp = WStamp { fileHash :: String
                         , replicaID :: ReplicaID    -- ^ The replicate that created this version of the file
                         , versionNum :: VersionNum  -- ^ The local version number of the replica that created this version of the file
                         } deriving (Show, Generic)

data DB = DB { dbReplicaID :: ReplicaID       -- ^ This replicas ID
             , dbVersionVec :: VersionVector
             , dbFileInfo :: FileInfo
             } deriving (Show, Generic)

instance ToJSON WriteStamp
instance FromJSON WriteStamp
instance ToJSON DB
instance FromJSON DB

loadDB :: IO DB
loadDB = do
  fileExists <- doesFileExist dbPath
  unless fileExists $ writeFile dbPath ""
  file <- BS.readFile dbPath
  gen <- getStdGen
  newReplicaID <- getStdRandom $ randomR (0, maxBound::Int)
  let json = decodeStrict file
      -- If the database file is empty or incorrectly formatted,
      -- then create a new DB with a new, randomly generated replicaID
      -- and a VersionVector containing this replica only with a version number of 1
      def = DB newReplicaID (M.singleton newReplicaID 1) M.empty
      db = fromMaybe def json
  return db

writeDB :: DB -> IO ()
writeDB db = BSL.writeFile dbPath $ encode db
