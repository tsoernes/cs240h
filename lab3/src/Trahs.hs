{-# OPTIONS -Wall #-}

module Trahs (
  trahs
) where

import           Control.Monad            (when)
import           Control.Monad.Fix            (fix)
import Control.Exception (SomeException (..), handle)
import           Data.Aeson               (decodeStrict,
                                           encode)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.Map.Strict          as M
import           Data.Maybe               (fromJust, fromMaybe)
import           System.Directory         (listDirectory,
                                           removeFile, renameFile)
import           System.Environment       (getArgs)
import           System.Exit              (exitFailure)
import           System.IO
import           System.PosixCompat.Files (getFileStatus, isRegularFile)

import           Utils

-- Communications protocol
-- (Local client wants to receive updates, if any, from remote server
-- then switch sides after one-way update is finished)
-- Database should be sent automatically when connection is established
-- and after turning.
-- Then client figures out what files it wants.
-- CLIENT: Requesting file: "fileA"
-- SERVER: <fileA contents>
-- CLIENT: Turning
-- SERVER: Requesting file: "fileB"
-- CLIENT: <fileB contents>

trahs :: IO ()
trahs = do
  -- Make stderr block on line write so that multiple threads can write
  -- at the same time without their output getting mangled together.
  hSetBuffering stderr LineBuffering
  args <- getArgs
  case args of
    ["--server", l] -> do hSetBuffering stdout LineBuffering
                          server stdin stdout l
    [r, l] | (host, ':':rdir) <- break (== ':') r -> connect host rdir l
    _ -> do hPutStrLn stderr "usage: trahs HOST:DIR LOCALDIR"
            exitFailure


-- | @server r w dir@ runs the code to serve the contents of @dir@,
-- reading input from @r@ and writing it to @w@.
server :: Handle -> Handle -> FilePath -> IO ()
server r w dir = do
  clogServ $ "Running on directory: " ++ dir
  sendDB w dir
  handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
    line <- hGetLine r
    case line of
      -- Switch roles
      "Turning" -> client False r w dir
      -- Process command and keep looping
      "Requesting file:" -> sendFile r w dir >> loop
      s -> clogServ $ "Unknown command: " ++ s


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
  clogServ $ "Running on directory: " ++ dir
  ldb <- scanDir dir
  rdb <- receiveDB r
  wss <- mergeState ldb rdb r w dir
  let vvec = updateVersionVec (dbVersionVec ldb) (dbVersionVec rdb)
  clogCli $ "Merged state: " ++ show wss
  clogCli $ "Updated version vec: " ++ show vvec
  let ldb' = DB (dbReplicaID ldb) vvec wss
  clogCli $ "Updated version vec: " ++ show vvec
  writeDB dir ldb'

  -- At the end, if `turn == True`, then we issue the TURN command to
  -- the server in order to swap roles and run `server r w dir` in
  -- order to upload changes to remote server.
  when turn $ do hPutStrLn w "Turning"
                 hPutStrLn stderr "Turning"
                 server r w dir


-- | First phase: Find modified files in the local directory and update the database.
scanDir :: FilePath -> IO DB
scanDir dir = do
  -- Read the directory and compute hashes for the files
  fileNames' <-  listDirectory dir
  let fileNames = filter (/= dbFileName) fileNames'
      filePaths' = fmap (\n -> dir +/+ n) fileNames
  hPutStrLn stderr $ "Detected files in directory " ++ dir ++ " :"
  _ <- sequence $ fmap (hPutStrLn stderr) filePaths'
  -- Only include regular files in DB. Exclude symbolic links, sub-directories etc.
  statuses <- sequence $ fmap getFileStatus filePaths'
  let filePaths = map fst $ filter (isRegularFile . snd) $ zip filePaths' statuses
  hashes <- mapM hashFile filePaths
  db <- loadDB dir
  let wss = dbWriteStamps db
      -- Update WriteStamps entries if the computed hashes does not match or are missing
      replicaID = dbReplicaID db
      versionNum = fromMaybe 1 $ M.lookup replicaID $ dbVersionVec db
      upd = updateWStamp replicaID versionNum
      wss' = foldr upd wss $ zip fileNames hashes
  hPutStrLn stderr $ "WSS in " ++ dir ++ " after scanDir: " ++ show wss'
  return $ db { dbWriteStamps = wss' }


-- | Update a WriteStamp in a WriteStamps if the given hash does not match
-- the recorded hash or the file does not have an entry.
updateWStamp :: ReplicaID -> VersionNum -> (FilePath, String) -> WriteStamps -> WriteStamps
updateWStamp replicaID versionNum (fName, fHash) wStamps =
  case M.lookup fName wStamps of
    Just WStamp { wsFileHash = fHash' } ->
      if fHash' == fHash
        then wStamps
        else upd
    _ -> upd
  where
    upd = M.alter (\_ -> Just $ WStamp fHash replicaID versionNum) fName wStamps


-- | Second phase: The server sends the client its database.
sendDB :: Handle -> FilePath -> IO ()
sendDB w dir = do
  db <- loadDB dir
  BSL.hPut w (encode db)
  hPutStrLn w ""
  -- hPutStrLn w (show $ encode db)
  -- hPutStrLn stderr ("Sent DB: " ++ show (encode db))


-- | Read a database from handle
receiveDB :: Handle -> IO DB
receiveDB r = do
  str <- BS.hGetLine r
  let db = fromJust $ decodeStrict str
  clogCli $ "Received DB: " ++ show db
  return db


-- | Parse file name from a download request and send the file
sendFile :: Handle -> Handle -> FilePath -> IO ()
sendFile r w dir = do
  fname <- hGetLine r
  clogServ $ "Sending file: " ++ dir +/+ fname
  file <- readFile $ dir +/+ fname
  hPutStrLn w file


-- | Send a request from client to server for file @fname@,
-- read it from handle and save to @dir@
receiveFile :: Handle -> Handle -> FilePath -> FilePath -> IO ()
receiveFile r w dir fname = do
  -- Request file from server
  hPutStrLn w "Requesting file:"
  hPutStrLn w fname
  file <- hGetLine r
  writeFile (dir +/+ fname) file


-- | Handle conflicting files by:
-- Delete original file. Create two new versions, named:
-- <fname>#<replicaID>.<versionNum>
-- Representing the versions from the two replicas client and server,
-- with their respective ReplicaID and version number from when
-- the conflict was handled.
handleConflict :: DB -> DB -> Handle -> Handle -> FilePath -> FilePath -> IO ()
handleConflict ldb rdb r w dir fname = do
  let newPath db = let fn = fname ++ "#" ++ show (dbReplicaID db) ++ "." ++ show (getVersionNum db)
                   in dir +/+ fn
  -- Rename client's (original) file
  renameFile (dir +/+ fname) $ newPath ldb
  -- Download file from server and rename it
  receiveFile r w dir fname
  renameFile (dir +/+ fname) $ newPath rdb

deleteFile :: FilePath -> FilePath -> IO ()
deleteFile dir fname = do
  clogCli $ "Deleting file: " ++ dir +/+ fname
  removeFile $ dir +/+ fname


-- | Merge write stamps of remote server into local database,
-- then execute the necessary actions, either download or delete file
-- in order to sync local state with remote state.
mergeState :: DB -> DB -> Handle -> Handle -> FilePath -> IO WriteStamps
mergeState ldb rdb r w dir = do
  -- hPutStrLn stderr $ "Mergin-- g ldb with rdb:"
  -- hPutStrLn stderr $ show ldb
  -- hPutStrLn stderr $ show rdb
  let syncActions = mergeWStamps ldb rdb
      syncIOActions = M.foldrWithKey' f [] syncActions
      f fname (sa, _) as = a:as
        where
          a = case sa of
            Download -> receiveFile r w dir fname
            Delete   -> deleteFile dir fname
            Conflict -> handleConflict ldb rdb r w dir fname
            Keep     -> return ()
  -- hPutStrLn stderr $ "Num. of sync actions" ++ show (length syncActions)
  -- hPutStrLn stderr $ "Num. of sync IO actions: " ++ show (length syncIOActions)
  _ <- sequence syncIOActions
  return $ M.map snd syncActions


-- | Third phase A: The client merges the write stamps of the remote server
-- into its own local database.
mergeWStamps :: DB -- ^ Local (client) DB
             -> DB -- ^ Remote (server) DB
             -> SyncActions -- Merged write stamps with according actions
mergeWStamps ldb rdb = M.unions [inBoth, servOnly, cliOnly]
  where
    lvv = dbVersionVec ldb -- Local version vector
    rvv = dbVersionVec rdb -- Remote version vector
    lwss = dbWriteStamps ldb  -- Local write stamps
    rwss = dbWriteStamps rdb  -- Remote write stamps

    inBoth = M.intersectionWith inBothF lwss rwss
    inBothF lws rws
      -- If the file exists on both the client and server
      -- and LWS = RWS, there is nothing to do.
      | lws == rws = (Keep, lws)
      -- If the files differ, but version(RWS) ≤ LVV!replica(RWS),
      -- then the client already learned about the server's version
      -- in some previous synchronization (and if less than,
      -- subsequently overwrote it). Hence, the client
      -- ignores the server's version and keeps its own with no change.
      | wsVersionNum rws <= verLookup (wsReplicaID rws) lvv = (Keep, lws)
      -- Conversely, if version(LWS) ≤ RVV!replica(LWS),
      -- then the server knew about and overwrote the client's version.
      -- Hence the client downloads the new version from the server,
      -- replaces the local file with the contents of the remote one
      | wsVersionNum lws <= verLookup (wsReplicaID lws) rvv = (Download, rws)
      -- If the file exists on both replicas and none of
      -- the above cases holds, flag a conflict.
      | otherwise = (Conflict, rws)

    -- If the file exists only on the server, then download it only if the
    -- client has not previously downloaded that version or a version that
    -- supersedes it (i.e., download only if version(RWS) > LVV!replica(RWS)).
    -- Otherwise, ignore the file as it was previously downloaded and deleted.
    servOnly = M.map (\ws -> (Download, ws)) $ M.filter servOnlyP $ M.difference rwss lwss
    servOnlyP rws = wsVersionNum rws > verLookup (wsReplicaID rws) lvv

    -- If the file exists only on the client, then delete it only if the server
    -- previously had the client's version of the file or a version derived
    -- from it. In other words, delete only if version(LWS) ≤ RVV!replica(LWS).
    -- cliOnly = M.map (\ws -> (Delete, ws)) $ M.filter cliOnlyP $ M.difference lwss rwss
    -- cliOnlyP lws = wsVersionNum lws <= verLookup (wsReplicaID lws) rvv
    cliOnly = M.map cliOnlyP $ M.difference lwss rwss
    cliOnlyP lws = if wsVersionNum lws <= verLookup (wsReplicaID lws) rvv
      then (Delete, lws)
      else (Keep, lws)


-- | Fourth phase: The client sets its version vector to contain the
-- element-wise maximum of its previous contents and the values in the
-- remote server's version vector, reflecting the fact that it knows
-- everything that the server knows.
updateVersionVec :: VersionVector -- ^ Local (client) version vector
                 -> VersionVector -- ^ Remote (server) version vector
                 -> VersionVector -- ^ Updated local version vector
updateVersionVec = M.unionWith max


-- | Connect to @host@ address and sync remote
-- directory @rdir@ with local directory @ldir@.
-- Once finished, @ldir@ on the client and @rdir@
-- on server @host@ should have the same contents.
connect :: String -> FilePath -> FilePath -> IO ()
connect host rdir ldir = do
  (r, w) <- spawnRemote host rdir
  client True r w ldir

