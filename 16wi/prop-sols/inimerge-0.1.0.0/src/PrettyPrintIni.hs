module PrettyPrintIni
    ( prettyPrint
    , prettyPrintSection
    , listIni
    -- * Low-level utility functions
    , buildVal, nl
    , toByteString
    ) where

-- Data.ByteString.Char8 and Data.ByteString both export the same
-- (strict) ByteString type
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as L
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as Map
import Data.Monoid

import Parser
import ParseIni

-- |Pretty-print an @INIFile@ in INI format.
prettyPrint :: INIFile -> S.ByteString
prettyPrint = toByteString . buildIni

prettyPrintSection :: INISection -> S.ByteString
prettyPrintSection = toByteString . buildSection

-- | Build a value, properly escaping characters that need it.  The
-- first argument, if 'True', always surrounds the value with double
-- quotes.  Otherwise, double quotes are only used if the string
-- contains whitespace or other non-printable characters.
buildVal :: Bool -> INIVal -> L.Builder
buildVal alwaysQuote v
  | needQuotes = quote <> foldMap escapeChar (S8.unpack v) <> quote
  | otherwise = L.byteString v
  where quote = L.char7 '"'
        escapeChar '\\' = L.string7 "\\\\"
        escapeChar '\n' = L.string7 "\\n"
        escapeChar '"' = L.string7 "\\\""
        escapeChar '\t' = L.string7 "\\t"
        escapeChar '\b' = L.string7 "\\b"
        escapeChar c = L.char8 c
        needQuotes
          | alwaysQuote = True
          | S.null v = False
          | isHSpace (S8.head v) || isHSpace (S8.last v) = True
          | otherwise = S8.any (`elem` ("#;\\\"\t\n\b" :: String)) v

nl :: L.Builder
nl = L.char7 '\n'

buildSubsectionName :: S.ByteString -> L.Builder
buildSubsectionName bs
  | S.null bs = mempty
  | special h = L.string7 ['\\', h] <> buildSubsectionName t
  | otherwise = L.byteString n <> buildSubsectionName r
  where special c = c == '\\' || c == '\"'
        Just (h,t) = S8.uncons bs
        (n,r) = S8.break special bs

buildSection :: INISection -> L.Builder
buildSection = Map.foldMapWithKey doNVS
  where doNVS n vs =
          let prefix = L.char7 '\t' <> L.byteString n <> L.string7 " = "
          in foldMap (\v -> prefix <> buildVal False v <> nl) vs

buildIni :: INIFile -> L.Builder
buildIni = Map.foldMapWithKey doSec
  where doSec s nvs = mconcat [ sectname s, buildSection nvs, nl ]
        sectname (ISect sec) = bracket $ L.byteString sec
        sectname (ISubsect sec subsec) = bracket $ mconcat [
          L.byteString sec, L.string7 " \"",
          buildSubsectionName subsec, L.char7 '\"' ]
        bracket x = mconcat [L.char7 '[', x, L.string7 "]\n"]
          
toByteString :: L.Builder -> S.ByteString
toByteString = mconcat . L.toChunks . L.toLazyByteString

-- | List the format of an INI file in the same format as `git config
-- --list`.  Note that this transformation is not reversible, as it
-- does weird things in the event that keys contain embedded newlines.
listIni :: INIFile -> S.ByteString
listIni = toByteString . Map.foldMapWithKey doSec
  where
    dot = L.char7 '.'
    iSecDot (ISect sec) = L.byteString sec <> dot
    iSecDot (ISubsect sec subsec) = mconcat [
      L.byteString sec, dot, L.byteString subsec, dot ]
    doSec isec0 = Map.foldMapWithKey inner
      where inner k0 vs =
              let k = isec <> L.byteString k0 <> L.char7 '='
              in foldMap (\v -> k <> L.byteString v <> nl) vs
            isec = iSecDot isec0
