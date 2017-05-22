-- | Run ParseIni on input file, pretty printing the results
module Main where

import ParseIni
import PrettyPrintIni

import qualified Data.ByteString as B
import System.Exit

-- |Main - parse input file, then pretty-print the result
main :: IO ()
main = do
    let result = parseIniFile B.empty
    either (\err -> putStrLn err >> exitFailure)
           (\success -> (B.putStr $ prettyPrint success) >> exitSuccess)
           result
