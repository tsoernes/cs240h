-- | Run ParseIni on input file, pretty printing the results
module Main where

import ParseIni
import PrettyPrintIni

import qualified Data.ByteString as B
import System.Exit

-- |Main - parse input file, then pretty-print the result
main :: IO ()
main = do
    -- take in input
    infile <- B.getContents

    -- run the parser on the input
    let result = parseIniFile infile
    
    -- run the pretty printer on the result if it succeeded
    either (\err -> putStrLn err >> exitFailure)
           (\success -> (B.putStr $ prettyPrint success) >> exitSuccess)
           result
