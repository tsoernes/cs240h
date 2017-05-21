{-# Language OverloadedStrings #-}

module ParseIni
    ( INISectName (..)
    , INIKey
    , INIVal (..)
    , INISection
    , INIFile
    , parseIniFile
    , toSectName
    , toKey
    , lookupSection
    , lookupValue
    ) where

import Control.Applicative
import Control.Monad (liftM)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import Data.Char (toLower)
import Data.Either (rights)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)

-- **** TYPES ****

-- |INI files are separated into sections and subsections of key-value pairs.
-- We represent section and subsection identifiers with the INISectName type.
-- Section names are case insensitive strings; subsection names are case sensitive.
data INISectName = ISect    { iSect    :: B.ByteString }
                 | ISubsect { iSect    :: B.ByteString
                            , iSubsect :: B.ByteString }
    deriving (Eq, Ord, Show)

-- |Within each (sub)section, an INI file contains a set of keys and values.
-- Keys are case insensitive strings.
type INIKey = B.ByteString

-- |After parsing key-value pairs, each value should be assigned a type.
-- We represent these types via the INIVal sum type.
data INIVal = IBool Bool
            | IInt Integer
            | IString B.ByteString
    deriving (Eq, Ord, Show)

-- |An @INISection@ is a map from @INIKey@s to @INIVal@s.
type INISection = M.Map INIKey [INIVal]

-- |An @INIFile@ is a map from @INISectName@s to @INISection@s.
type INIFile = M.Map INISectName INISection




-- **** INTERFACE ****

-- |Given a section name and possibly a subsection name, return an
-- appropriate @INISectName@. This function accounts for the case
-- insensitivity of the section name.
toSectName :: String -> Maybe String -> INISectName
toSectName s Nothing = ISect . B.pack $ map toLower s
toSectName s (Just ss) = ISubsect (B.pack $ map toLower s) (B.pack ss)

-- |Given a key name, return an appropriate @INIKey@. This function
-- accounts for the case insensitivity of the key name.
toKey :: String -> INIKey
toKey = B.pack . (map toLower)

-- |Look up a section in an @INIFile@.
lookupSection :: INISectName -> INIFile -> Maybe INISection
lookupSection = M.lookup

-- |Look up a value in an @INISection@.
lookupSValue :: INIKey -> INISection -> Maybe [INIVal]
lookupSValue = M.lookup

-- |Look up a value in an @INIFile@.
lookupValue :: INIKey -> INISectName -> INIFile -> Maybe [INIVal]
lookupValue k s m = lookupSection s m >>= \x -> lookupSValue k x




-- **** PARSER ****

--   **
--   ** Parsing an INI file. **
--   **

-- |Parse an INI file.
--
-- An INI file comprises a sequence of sections.
--
-- A section starts with a header, which declares the name of the section or subsection.
-- The header is followed by a sequence of key-value declarations.
--
-- Whitespace between and inside sections is ignored, as are comment lines, which
-- begin with @#@ or @;@.
parseIniFile :: B.ByteString -> Either String INIFile
parseIniFile = A.parseOnly pIniFile

-- |Parse an INI file.
--
-- An INIFile is a sequence of sections; @many pIniSection@ returns
-- a list of such sections, and @( pWSOrComment >> A.endOfInput )@
-- ensures that the parser has consumed all of the input.
pIniFile :: A.Parser INIFile
pIniFile = many pIniSection <* ( pWSOrComment >> A.endOfInput ) >>=
           \sects -> return $ foldl' addSec M.empty sects
  where addSec m (sh, sk) = M.insert sh updMap m
          where secMap = M.findWithDefault M.empty sh m
                updMap = foldl' addKVs secMap sk
        addKVs m (k, v) = M.insertWith insHelp k [v] m
          where insHelp _ o = v : o

-- |Parse one section of an INI file.
pIniSection :: A.Parser (INISectName, [(INIKey, INIVal)])
pIniSection = do pWSOrComment
                 header <- pIniSectHead
                 keyvals <- many pIniKeyValue
                 return (header, keyvals)



--   **
--   ** Parsing section or subsection headers. **
--   **

-- |Parse a section or subsection header.
--
-- A section header comprises @[sectName]@, and is case insensitive.
--
-- A subsection header comprises @[sectName "subsectName"]@. @sectName@
-- is case insensitive, while @subsectName@ is case sensitive.
--
-- A subsection name is enclosed in double quotes. It may contain any
-- character except newline; double quote and backslash must be escaped.
pIniSectHead :: A.Parser INISectName
pIniSectHead = pSkipSpace >> ( pIniSect <|> pIniSubsect )

-- |A section is @[foo]@
pIniSect = do A.char '['
              pSkipSpace
              sSec <- A.takeWhile1 isSectChar
              pSkipSpace
              A.char ']'
              return ISect { iSect = toCI sSec }

-- |Allowed section characters are alphanumeric plus @.@ and @-@.
isSectChar = A.inClass "0-9a-zA-Z.-"

-- |A subsection decl is @[foo "bar"]@.
pIniSubsect = do A.char '['
                 pSkipSpace
                 sSec <- A.takeWhile1 isSectChar
                 pSkipSpace
                 sSub <- quotedSubsect
                 pSkipSpace
                 A.char ']'
                 return ISubsect { iSect = toCI sSec, iSubsect = sSub }

-- |Parse the quoted portion of a subsection declaration.
quotedSubsect = quotedFromParser quotedSsChar

-- |Parse either a sequence of valid subsection chars or an escaped char.
quotedSsChar = A.takeWhile1 isSubsectChar
           <|> pEscChar ["\\", "\"", "t", "b"]

-- |All characters are allowed inside quotes except newline, double quote, and backslash.
isSubsectChar c = c /= '\r' && c /= '\n' && c /= '\\' && c /= '\"'



--   **
--   ** Parsing key=value declarations. **
--   **

-- |Parse a key=value declaration.
--
-- There are two allowed forms. In the first, @key@ appears alone; this is
-- equivalent to declaring @key = true@. The second form is @key = value@.
--
-- Keys are case insensitive and can contain alphanumerics and @-@; they must
-- begin with an alphabetical character.
pIniKeyValue :: A.Parser (INIKey, INIVal)
pIniKeyValue = pWSOrComment >> ( pIniKeyOnly <|> pIniKeyVal )

-- |Parse a key-only declaration, which is equivalent to @key = true@.
pIniKeyOnly = pIniKeyName <* pEndOfValue >>= \name -> return (name, IBool True)

-- |Parse a @key = value@ declaration.
pIniKeyVal = do key <- pIniKeyName
                pSkipSpace
                A.char '='
                pSkipSpaceQuotNl
                val <- pIniVal
                return (key, val)

-- |A valid key character is alphanumeric or @-@.
isKeyChar = A.inClass "0-9A-Za-z-"

-- |A key name starts with an alpha char and is then zero or more valid key characters.
pIniKeyName = do pSkipSpace
                 ini <- A.letter_ascii
                 rest <- A.takeWhile isKeyChar
                 return . toCI $ B.cons ini rest

-- |A value is a bool, an optionally signed integer, or a string.
pIniVal = ( pBoolVal <* pEndOfValue )
      <|> ( pIntVal <* pEndOfValue )
      <|> ( pStringVal <* pEndOfValue )

-- |Bools can be denoted with "yes/no", "true/false", or "on/off".
pBoolVal = liftM IBool $
             ( A.stringCI "yes" <|> A.stringCI "true" <|> A.stringCI "on" >> return True )
         <|> ( A.stringCI "no" <|> A.stringCI "false" <|> A.stringCI "off" >> return False )

-- |Integers can be signed, and can take optional suffixes @k@, @M@, @G@, @T@,
-- @P@, or @E@, indicating scaling by 2^10, 2^20, etc.
pIntVal = do ival <- A.signed A.decimal
             isuf <- A.option 1 pSuffix
             return . IInt $ ival * isuf
  where pSuffix = liftM lookupSuffix suffixChoice
        suffixChoice = A.choice $ map A.char suffixes
        suffixes = ['k', 'M', 'G', 'T', 'P', 'E']
        suffAssoc = zip suffixes $ map (2^) ([10, 20, 30, 40, 50, 60] :: [Integer])
        lookupSuffix = fromJust . flip lookup suffAssoc
        -- It's safe to use fromJust here because we're guaranteed to get only values
        -- from `suffixes` out of suffixChoice.

-- |A string value can be partially or fully quoted. Internal whitespace
-- is retained, but trailing and leading whitespace is discarded.
pStringVal = liftM (IString . B.concat) $ many valMaybeLeadingWS

-- |Parse a portion of a value decl with leading whitespace.
--
-- We require that whitespace is always followed by a non-whitespace
-- section because unquoted trailing whitespace should be discarded;
-- thus, this parser will never conume trailing whitespace.
valMaybeLeadingWS = do ws <- takeWhitespace
                       val <- quotedValue <|> unquotedValue
                       return $ B.append (B.concat ws) val
        -- takeWhitespace pulls whitespace even when broken by line continuations.
        -- This is important in the case that a value ends with whitespace that is
        -- itself broken across a continued newline. This is trailing whitespace,
        -- and it must be trimmed from the value.
  where takeWhitespace = liftM rights $ many ( takeSpaceNotNl <|> takeQuotNl )
        takeSpaceNotNl = liftM Right $ A.takeWhile1 isSpaceNotNl
        takeQuotNl = liftM Left $ pQuotNLIsh

-- |Parse a quoted portion of a value decl.
quotedValue = quotedFromParser $ escValChar isQuValChar

-- |Parse an unquoted portion of a value decl.
unquotedValue = A.many1 ( escValChar isUnqValChar ) >>= \subs -> return $ B.concat subs

-- |Parse either a sequence of valid value chars or an escaped char.
escValChar p = A.takeWhile1 p
           <|> pEscChar ["\\", "\"", "\r\n", "\r", "\n", "n", "t", "b", "r"]

-- |Everything except double quote and backslash can be in the quoted portion of a value.
isQuValChar c = c /= '"' && c /= '\\'

-- |An unquoted portion of a value may not contain quotes or comment chars.
--
-- We also define unquoted portions to not contain whitespace; this is because
-- interstitial whitespace is handled in the @valMaybeLeadingWS@ parser, above.
isUnqValChar = A.notInClass "\"\\#;\r\n \t\v"



--   **
--   ** Utility functions **
--   **

-- |Section names and keys are stored as case-insensitive strings.
-- toCI just lowercases the given string.
toCI = B.map toLower

-- |For our purposes, EoL should include EoF
pEoL = A.endOfInput <|> A.endOfLine

-- |Leniency with line continuations: in addition to matching @"\\\n"@,
-- we allow @"\\\r\n"@ and @"\\\r"@.
pQuotNLIsh = A.choice $ map A.string ["\\\r\n", "\\\r", "\\\n"]

-- |Skip spaces other than end-of-line characters.
pSkipSpace = A.skipWhile isSpaceNotNl

-- |Skip spaces other than end-of-line characters, and also skip quoted newlines.
pSkipSpaceQuotNl = A.skipMany ( skipSpc <|> skipQNl )
  where skipSpc = A.takeWhile1 isSpaceNotNl >> return ()
        skipQNl = pQuotNLIsh >> return ()

-- |A predicate for spaces other than newline-like things.
--
-- We don't use @isSpace@ because it includes @\\r@ and @\\n@,
-- while we consider these separately.
isSpaceNotNl c = c == ' ' || c == '\t' || c == '\v' || c == '\f'

-- |Comments start with @#@ or @;@ and go to end of line.
pComment = ( A.char '#' <|> A.char ';' ) >> pToEOL
  where pToEOL = pEoL <|> ( A.take 1 >> pToEOL )
        -- equivalently, A.manyTill A.anyChar pEoL >> return ()

-- |Accept a comment, the end of the line, or the end of input.
pComOrEoL = pComment <|> A.endOfLine <|> A.endOfInput

-- |Consume all whitespace or comments to the end of the line or EoF. No escaped newlines.
pWSOrComment = A.skipMany $ A.peekChar' >> ( pSkipSpace >> pComOrEoL )

-- |Consume whitespace or comments to end of line, absorbing escaped newlines; or find an EoF.
pEndOfValue = pSkipSpaceQuotNl >> pComOrEoL

-- |Consume a quoted section, given a parser for valid characters
-- inside the quoted region.
quotedFromParser p = do A.char '"'
                        substrs <- many p
                        A.char '"'
                        return $ B.concat substrs

-- |Given a list of valid escape characters, parse an escape sequence,
-- returning the escaped character.
pEscChar escs = A.char '\\' >> A.choice escs >>= \esc -> return $ mapEsc esc
  where mapEsc "n" = "\n"
        mapEsc "t" = "\t"
        mapEsc "b" = "\b"
        mapEsc "r" = "\r"
        mapEsc "\r\n" = ""      -- escaped newlines are just dropped
        mapEsc "\r" = ""        -- "
        mapEsc "\n" = ""        -- "
        mapEsc  x  =  x
