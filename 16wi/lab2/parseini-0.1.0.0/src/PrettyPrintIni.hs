module PrettyPrintIni
    ( prettyPrint
    ) where

import ParseIni

-- Data.ByteString.Char8 and Data.ByteString both export the same (strict) ByteString type
import qualified Data.ByteString.Char8 as BC

-- |Pretty-print an @INIFile@ in INI format.
prettyPrint :: INIFile -> BC.ByteString
prettyPrint = const $ BC.pack ""
