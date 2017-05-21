{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString as S
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import MergeIni
import ParseIni
import PrettyPrintIni

readIniFile :: FilePath -> IO INIFile
readIniFile file = S.readFile file >>= check . parseIniFile
  where check (Right ini) = return ini
        check (Left msg) = fail $ file ++ ": " ++ msg

merge :: FilePath -> FilePath -> FilePath -> IO S.ByteString
merge headFile baseFile mergeFile = do
  headIni <- readIniFile headFile
  baseIni <- readIniFile baseFile
  mergeIni <- readIniFile mergeFile
  let newHead = mergeINI headIni baseIni mergeIni
  return $ prettyPrint newHead

data Options = Options {
    optOverwriteHead :: Bool
  } deriving (Show)

runMerge :: Options -> FilePath -> FilePath -> FilePath -> IO ()
runMerge opt headFile baseFile mergeFile =
  merge headFile baseFile mergeFile >>= writeout
  where writeout | optOverwriteHead opt = S.writeFile headFile
                 | otherwise            = S.putStr

defaultOptions :: Options
defaultOptions = Options {
    optOverwriteHead = False
  }

options :: [OptDescr (Options -> Options)]
options = [
    Option "w" []
           (NoArg $ \o -> o { optOverwriteHead = True })
           "Overwrite head file with merged file"
  ]

usage :: [String] -> IO a
usage errors = do
  prog <- getProgName
  mapM_ (hPutStr stderr) errors
  let summary = "usage: " ++ prog ++ " OPTIONS headFile baseFile mergeFile"
  hPutStrLn stderr $ usageInfo summary options
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case getOpt RequireOrder options args of
    (o, [h,b,m], []) -> runMerge (foldl (flip ($)) defaultOptions o) h b m
    (_, _, e) -> usage e

