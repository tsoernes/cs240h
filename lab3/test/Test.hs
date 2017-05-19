module Main where

import Test.Hspec
import System.Environment (setEnv, withArgs)
import System.Directory
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Trahs
import Utils

dirA, dirB :: String
dirA = "/home/torstein/sync/A"
dirB = "/home/torstein/sync/B"

main :: IO ()
main = hspec $ before_ clean $ describe "Testing Lab 3" $
  describe "main" $
    it "is main" $ do
      writeFile (dirB +/+ "fileA") "Contents of fileA"
      writeFile (dirB +/+ "fileB") "Contents of fileB"
      x <- withArgs ["irrelevant:/home/torstein/sync/A", "/home/torstein/sync/B"] trahs


      -- Add file to dirA and run same command again,
      -- the file should then be synced to dirB after turn.
      -- Then recheck the above.
      x `shouldBe` ()

  -- example quickcheck test in hspec.
  -- describe "read" $ do
  --   it "is inverse to show" $ property $
  --     \x -> (read . show) x == (x :: Int)

isSynced :: Spec
isSynced = beforeAll loadDBs $
  describe "The two directories" $ do

    aroundWith (\_ -> \_ -> return ()) sameFiles

    it "Has the same write stamps" $ \(dbA, dbB) ->
      dbWriteStamps dbA `shouldBe` dbWriteStamps dbB

    it "Has a write stamp for each file and vice versa" $ \(dbA, _) -> do
      fnames <- getDirectoryContents dirA
      let wss = dbWriteStamps dbA
      M.keysSet wss `shouldBe` S.fromList fnames

    it "Have the same version vectors" $ \(dbA, dbB) ->
      -- Assert that version vectors are equal
      dbVersionVec dbA `shouldBe` dbVersionVec dbB


sameFiles :: Spec
sameFiles =
  describe "Files" $
    it "has same file names and contents" $ do
      fnamesA' <- getDirectoryContents dirA
      fnamesB' <- getDirectoryContents dirB
      let fnamesA = filter (/= dbFileName) fnamesA'
          fnamesB = filter (/= dbFileName) fnamesB'
      fnamesA `shouldBe` fnamesB
      contentsA <- sequence $ fmap readFile fnamesA
      contentsB <- sequence $ fmap readFile fnamesB
      contentsB `shouldBe` contentsA


loadDBs :: IO (DB, DB)
loadDBs = do
  dbA <- loadDB dirA
  dbB <- loadDB dirB
  return (dbA, dbB)

clean :: IO ()
clean = do
  setEnv "TRASSH" "$HOME/trahs --server"
  -- Delete all existing files
  inA <- getDirectoryContents dirA
  inB <- getDirectoryContents dirB
  mapM_ removeFile inA
  mapM_ removeFile inB
  return ()
