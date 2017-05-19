{-# OPTIONS -Wall #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

-- For writing and reading DB to file with Aeson
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


module Utils where

import           Codec.Digest.SHA     (Length (SHA256), hash, showBSasHex)
import           Control.Monad        (unless)
import           Data.Aeson           (FromJSON, ToJSON, decodeStrict, encode)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict      as M
import           Data.Maybe           (fromMaybe)
import           GHC.Generics         (Generic)
import           System.Directory     (doesFileExist)
import           System.Environment   (lookupEnv)
import           System.IO
import           System.Process
import           System.Random        (getStdGen, getStdRandom, randomR)

dbFileName :: String
dbFileName = ".trahs.db"

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

data SyncAction = Download | Keep | Delete | Conflict
type WriteStamps = M.Map FilePath WriteStamp
type SyncActions = M.Map FilePath (SyncAction, WriteStamp)


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


-- | Look up a ReplicaID in a VersionVector to get a VersionNum, default to 0
verLookup :: ReplicaID -> VersionVector -> VersionNum
verLookup = M.findWithDefault 0


-- | Get version of given database
getVersionNum :: DB -> VersionNum
getVersionNum db = verLookup (dbReplicaID db ) (dbVersionVec db)


-- | Calculate a hash to uniquely identify a file
hashFile :: FilePath -> IO String
hashFile path = showBSasHex <$> (hash SHA256 <$> BSL.readFile path)


 -- | Load a database with the default name from the given directory.
 -- If it fails, creates a default DB.
loadDB :: FilePath -> IO DB
loadDB dir = do
  let dbPath = dir +/+ dbFileName
  fileExists <- doesFileExist dbPath
  unless fileExists $ writeFile dbPath "" -- Touch empty DB file
  file <- BS.readFile dbPath
  defDB <- emptyDB
  let json = decodeStrict file
      -- If the database file is empty or incorrectly formatted,
      -- then create a new DB with a new, randomly generated replicaID
      -- and a VersionVector containing this replica only with a version number of 1
      db = fromMaybe defDB json
  return db

emptyDB :: IO DB
emptyDB = do
  gen <- getStdGen
  newReplicaID <- getStdRandom $ randomR (0, maxBound::Int)
  let def = DB newReplicaID (M.singleton newReplicaID 1) M.empty
  return def


-- | Write DB to given directory with default name
writeDB :: FilePath -> DB -> IO ()
writeDB dir db = BSL.writeFile (dir +/+ dbFileName) $ encode db


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
  -- hPutStrLn stderr $ "running " ++ show cmd
  (Just w, Just r, _, _) <- createProcess (shell cmd) {
        std_in = CreatePipe
      , std_out = CreatePipe
    }
  hSetBuffering w LineBuffering
  return (r, w)


-- | Command for executing trahs on a remote system.  The '@' will be
-- replaced by the hostname, and the directory will be appended.
traSSH :: String
traSSH = "ssh -CTaxq @ ./trahs --server"


(+/+) :: FilePath -> FilePath -> FilePath
(+/+) dir name = dir ++ "/" ++ name

clog :: String -> IO ()
clog s = hPutStrLn stderr s

clogServ :: String -> IO ()
clogServ s = clog $ "Server: " ++ s

clogCli :: String -> IO ()
clogCli s = clog $ "Client: " ++ s
