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
    it "Test 1" $ do
      writeFile (dirB +/+ "fileA") "Contents of fileA"
      writeFile (dirB +/+ "fileB") "Contents of fileB"
      _ <- withArgs ["irrelevant:/home/torstein/sync/A", "/home/torstein/sync/B"] trahs
      hspec isSynced

      writeFile (dirB +/+ "fileC") "Contents of fileC"
      _ <- withArgs ["irrelevant:/home/torstein/sync/A", "/home/torstein/sync/B"] trahs
      hspec isSynced

      writeFile (dirA +/+ "fileD") "Contents of fileD"
      _ <- withArgs ["irrelevant:/home/torstein/sync/A", "/home/torstein/sync/B"] trahs
      hspec isSynced
      -- TODO: Add file to dirA and run same command again,
      -- the file should then be synced to dirB after turn.
      -- Then recheck the above.

  -- example quickcheck test in hspec.
  -- describe "read" $ do
  --   it "is inverse to show" $ property $
  --     \x -> (read . show) x == (x :: Int)


isSynced :: Spec
isSynced = beforeAll loadDBs $
  describe "The two directories" $ do
    aroundWith (\a _ -> a ()) sameFiles

    it "Has the same write stamps" $ \(dbA, dbB) ->
      dbWriteStamps dbA `shouldBe` dbWriteStamps dbB

    it "Has a write stamp for each file and vice versa" $ \(dbA, _) -> do
      fnames' <- listDirectory dirA
      let fnames = filter (/= dbFileName) fnames'
          wss = dbWriteStamps dbA
      M.keysSet wss `shouldBe` S.fromList fnames

    it "Has the same version vectors" $ \(dbA, dbB) ->
      dbVersionVec dbA `shouldBe` dbVersionVec dbB


sameFiles :: Spec
sameFiles =
  it "Has the same files (same names and contents)" $ do
    fnamesA' <- listDirectory dirA
    fnamesB' <- listDirectory dirB
    let fnamesA = filter (/= dbFileName) fnamesA'
        fnamesB = filter (/= dbFileName) fnamesB'
    fnamesA `shouldBe` fnamesB
    contentsA <- sequence $ fmap (\p -> readFile $ dirA +/+ p) fnamesA
    contentsB <- sequence $ fmap (\p -> readFile $ dirB +/+ p)fnamesB
    contentsA `shouldBe` contentsB


loadDBs :: IO (DB, DB)
loadDBs = do
  dbA <- loadDB dirA
  dbB <- loadDB dirB
  return (dbA, dbB)


-- | Delete _all_ files in the two sync directories
clean :: IO ()
clean = do
  setEnv "TRASSH" "$HOME/trahs --server"
  -- Delete all existing files
  inA <- listDirectory dirA
  inB <- listDirectory dirB
  mapM_ (\p -> removeFile $ dirA +/+ p) inA
  mapM_ (\p -> removeFile $ dirB +/+ p) inB
  return ()
