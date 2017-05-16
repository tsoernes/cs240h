{-# OPTIONS -Wall #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- For writing and reading DB to file with Aeson
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- Easy updating of records
{-# LANGUAGE NamedFieldPuns    #-}

module Trahs (
  trahs
) where

import           Codec.Digest.SHA     (Length (SHA256), hash, showBSasHex)
import           Control.Monad        (unless, when)
import           Data.Aeson           (FromJSON, ToJSON, decodeStrict, encode)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict      as M
import           Data.Maybe           (fromJust, fromMaybe)
import           GHC.Generics         (Generic)
import           System.Directory     (doesFileExist, listDirectory)
import           System.Environment   (getArgs, lookupEnv)
import           System.Exit          (exitFailure)
import           System.IO
import           System.Process
import           System.Random        (getStdGen, getStdRandom, randomR)

dbFileName :: String
dbFileName = ".trahs.db"

trahs :: IO ()
trahs = do
  args <- getArgs
  case args of
    ["--server", l] -> do hSetBuffering stdout LineBuffering
                          server stdin stdout l
    [r, l] | (host, ':':rdir) <- break (== ':') r -> connect host rdir l
    _ -> do hPutStrLn stderr "usage: trahs HOST:DIR LOCALDIR"
            exitFailure


-- | An ID unique for a specific computer and directory, generated
-- randomly the first time trahs is run on a directory.
type ReplicaID = Int

-- | A local version number starts at 1 and increments every time
-- trahs is run (and server is connectable).
type VersionNum = Int

-- | A version vector is a map from replica ID to version number.
-- A replica's own replica ID always maps to its latest local version number.
-- Conceptually, any replica IDs not in the version vector are mapped to version number 0.
type VersionVector = M.Map ReplicaID VersionNum

data WriteStamp = WStamp
  { wsFileHash   :: String        -- ^ Used to check whether a file has changed
  , wsReplicaID  :: ReplicaID    -- ^ The replica that created this version of the file
  -- | The local version number of the replica that created this version of the file
  , wsVersionNum :: VersionNum
  } deriving (Show, Generic, Eq)

type WriteStamps = M.Map FilePath WriteStamp

data DB = DB
  { dbReplicaID   :: ReplicaID       -- ^ This replica's ID
  , dbVersionVec  :: VersionVector
  , dbWriteStamps :: WriteStamps
  } deriving (Show, Generic)

-- For serializing and deserializing when writing or reading to/from stdin/stout and file.
instance ToJSON WriteStamp
instance FromJSON WriteStamp
instance ToJSON DB
instance FromJSON DB


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
    "TURN" -> hPutStrLn w "=== switching from client to server===" >> client False r w dir
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
  when (line == "DB") $ receiveDB dir r
  hPutStrLn stderr $ "The server said " ++ line
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


-- | First phase: Find modified files in the local directory and update the database.
scanDir :: FilePath -> IO ()
scanDir path = do
  -- Read the directory and compute hashes for the files
  fileNames <- listDirectory path
  hashes <- mapM hashFile fileNames
  db <- loadDB path
  let wss = dbWriteStamps db
      -- Update WriteStamps entries if the computed hashes does not match or are missing
      replicaID = dbReplicaID db
      versionNum = fromMaybe 1 $ M.lookup replicaID $ dbVersionVec db
      upd = updateVVec replicaID versionNum
      wss' = foldr upd wss $ zip fileNames hashes
  writeDB path $ db { dbWriteStamps = wss' }


-- | Update a WriteStamp in a WriteStamps if the given hash does not match
-- the recorded hash or the file does not have an entry.
updateVVec :: ReplicaID -> VersionNum -> (FilePath, String) -> WriteStamps -> WriteStamps
updateVVec replicaID versionNum (fName, fHash) wStamps =
  case M.lookup fName wStamps of
    Just WStamp { wsFileHash = fHash' } ->
      if fHash' == fHash
        then wStamps
        else upd
    _ -> upd
  where
    upd = M.adjust (\ws -> WStamp fHash replicaID versionNum) fName wStamps


-- | Second phase: The server sends the client its database.
sendDB :: FilePath -> Handle -> IO ()
sendDB path w = do
  db <- loadDB path
  hPutStrLn w "DB"
  BSL.hPut w (encode db)
  hPutStrLn w ""


-- | Read a database from StdIn and write it to file
receiveDB :: FilePath -> Handle -> IO ()
receiveDB path r = do
  str <- BS.hGetLine r
  let db = fromJust $ decodeStrict str
  writeDB path db


-- | Third phase: The client merges the database of the remote server
-- into its own local database.
mergeState :: DB -- ^ Local (client) DB
           -> DB -- ^ Remote (server) DB
           -> DB -- ^ Merged local DB
mergeState ldb rdb = undefined -- DB { dbReplicaID = dbReplicaID ldb, dbVersionVec = lvv', dbWriteStamps = lwss'}
  where
    lvv = dbVersionVec ldb -- Local version vector
    rvv = dbVersionVec rdb -- Remote version vector
    lwss = dbWriteStamps ldb  -- Local write stamps
    rwss = dbWriteStamps rdb  -- Remote write stamps

    inBoth = M.intersectionWith inBothF lwss rwss
    inBothF lws rws = if lws == rws
      -- If the file exists on both the client and server and LWS = RWS, there is nothing to do.
      then lws -- Files are identical
      -- If the files differ, but version(RWS) ≤ LVV!replica(RWS),
      else if wsVersionNum rws <= verLookup (wsReplicaID rws) lvv
        -- then the client already learned about the server's version in some previous
        -- synchronization (and if less than, subsequently overwrote it). Hence, the client
        -- ignores the server's version and keeps its own with no change.
        then lws -- Client has newest version
        -- Conversely, if version(LWS) ≤ RVV!replica(LWS),
        else if wsVersionNum lws <= verLookup (wsReplicaID lws) rvv
          -- then the server knew about and overwrote the client's version.
          -- Hence the client downloads the new version from the server,
          -- replaces the local file with the contents of the remote one
          then rws -- Server has newest version; should download
          -- If the file exists on both replicas and none of
          -- the above cases holds, flag a conflict.
          else rws -- TODO: Conflict

    -- If the file exists only on the server, then download it only if the
    -- client has not previously downloaded that version or a version that
    -- supersedes it (i.e., download only if version(RWS) > LVV!replica(RWS)).
    -- Otherwise, ignore the file as it was previously downloaded and deleted.
    servOnly = M.filter servOnlyF $ M.difference rwss lwss
    servOnlyF rws = wsVersionNum rws > verLookup (wsReplicaID rws) lvv

    -- If the file exists only on the client, then delete it only if the server
    -- previously had the client's version of the file or a version derived
    -- from it. In other words, delete only if version(LWS) ≤ RVV!replica(LWS).
    cliOnly = M.filter cliOnlyF $ M.difference lwss rwss
    cliOnlyF lws = wsVersionNum lws > verLookup (wsReplicaID lws) rvv


-- | Look up a ReplicaID in a VersionVector to get a VersionNum, default to 0
verLookup :: ReplicaID -> VersionVector -> VersionNum
verLookup = M.findWithDefault 0


-- | Fourth phase: The client sets its version vector to contain the
-- element-wise maximum of its previous contents and the values in the
-- remote server's version vector, reflecting the fact that it knows
-- everything that the server knows.
updateVersionVec :: VersionVector -- ^ Local (client) version vector
                 -> VersionVector -- ^ Remote (server) version vector
                 -> VersionVector -- ^ Updated local version vector
updateVersionVec = M.unionWith max

 -- | Load a database with the default name from the given directory.
 -- If it fails, creates a default DB.
loadDB :: FilePath -> IO DB
loadDB path = do
  let dbPath = path ++ dbFileName
  fileExists <- doesFileExist dbPath
  unless fileExists $ writeFile dbPath "" -- Touch empty DB file
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

-- | Write DB to given path with default name
writeDB :: FilePath -> DB -> IO ()
writeDB path db = BSL.writeFile (path ++ dbFileName) $ encode db
