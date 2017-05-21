module PrettyPrintIni
    ( prettyPrint
    ) where

import ParseIni

import Data.ByteString.Builder
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as B
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Monoid

-- |Pretty-print an @INIFile@ in INI format.
prettyPrint :: INIFile -> BC.ByteString
prettyPrint = B.toStrict . toLazyByteString . ppFile

-- |Pretty-Print an @INIFile@.
ppFile :: INIFile -> Builder
ppFile = M.foldrWithKey ppHlp mempty
  where ppHlp k s m = ppSectName k <> ppSection s <> m

-- |Pretty-print an @INISection@.
ppSection :: INISection -> Builder
ppSection = M.foldrWithKey ppKeyVals mempty
  where ppKeyVals k v m = foldl' (ppHlp k) m v
        ppHlp k m v = ppKeyVal k v <> m

-- |Pretty-print a key and value pair
ppKeyVal :: INIKey -> INIVal -> Builder
ppKeyVal k v = string8 "    " <> byteString k <> string8 " = " <> ppVal v <> char8 '\n'

-- |Pretty-print an @INISectName@ in INI format.
ppSectName :: INISectName -> Builder
ppSectName (ISect s) = char8 '[' <> byteString s <> string8 "]\n"
ppSectName (ISubsect s ss) = char8 '[' <> byteString s <> BC.foldl' ppEsc sBeg ss <> sEnd
  where sBeg = string8 " \""
        sEnd = string8 "\"]\n"

-- |Pretty-print an @INIVal@ in INI format.
ppVal :: INIVal -> Builder
ppVal (IBool b) = string8 $ show b
ppVal (IInt i) = integerDec i
ppVal (IString s) = BC.foldl' ppEsc (char8 '"') s <> char8 '"'

-- |Handle escaping for characters that need it.
ppEsc :: Builder -> Char -> Builder
ppEsc m '\\' = m <> char8 '\\' <> char8 '\\'
ppEsc m '\"' = m <> char8 '\\' <> char8 '\"'
ppEsc m '\t' = m <> char8 '\\' <> char8 't'
ppEsc m '\b' = m <> char8 '\\' <> char8 'b'
ppEsc m '\n' = m <> char8 '\\' <> char8 'n'
ppEsc m '\r' = m <> char8 '\\' <> char8 'r'
ppEsc m c = m <> char8 c
