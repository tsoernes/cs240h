-- | Run Haskell tr implementation.
--
-- We will be testing using your `Tr` module, not `Main`, so don't worry too
-- much about compatibility of argument parsing.
module Main where

import System.Environment

import Tr

-- | Main - parse args, and read from stdin.
main :: IO ()
main = do
  args <- getArgs
  inp <- readLn
  let res = case args of
        ["-d", cs] -> tr cs Nothing inp
        [csIn, csOut] -> tr csIn (Just csOut) inp
        _ -> "Invalid arguments"
  putStrLn res

