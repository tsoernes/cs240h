{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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

import Data.Char
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)

import Parser

-- **** TYPES ****
-- These are the types you should use for the results of your parse.
-- Think carefully about what's going on here!

-- | INI files are separated into sections and subsections of key-value pairs.
-- We represent section and subsection identifiers with the INISectName type.
-- Section names are case insensitive strings; subsection names are case sensitive.
data INISectName = ISect    { iSect    :: B.ByteString }
                 | ISubsect { iSect    :: B.ByteString
                            , iSubsect :: B.ByteString }
    deriving (Eq, Ord, Show)

-- | Within each (sub)section, an INI file contains a set of keys and values.
-- Keys are case insensitive strings.
type INIKey = B.ByteString

-- | After parsing key-value pairs, each value should be assigned a type.
-- We represent these types via the @INIVal@ sum type.
data INIVal = IBool Bool
            | IInt Integer
            | IString B.ByteString
    deriving (Eq, Ord, Show)

-- | An @INISection@ is a map from @INIKey@s to @INIVal@s.
type INISection = M.Map INIKey [INIVal]

-- | An @INIFile@ is a map from @INISectName@s to @INISection@s.
type INIFile = M.Map INISectName INISection


-- **** INTERFACE ****
-- You need to implement these so that we can test your code!
--
-- Why? Because you shouldn't need to expose exactly the way that
-- you handle, e.g., case insensitive string matching in order for
-- someone to use your INI file parser.

-- | Given a section name and possibly a subsection name, return an
-- appropriate @INISectName@. This function accounts for the case
-- insensitivity of the section name.
toSectName :: String -> Maybe String -> INISectName
toSectName sect subsect = let sect' = C.pack $ map toLower sect
  in case subsect of
       Just subsect' -> ISubsect sect' (C.pack subsect')
       Nothing -> ISect sect'


-- | Given a key name, return an appropriate @INIKey@. This function
-- accounts for the case insensitivity of the key name.
toKey :: String -> INIKey
toKey = C.pack . map toLower


-- | Look up a section in an @INIFile@.
lookupSection :: INISectName -> INIFile -> Maybe INISection
lookupSection = undefined


-- | Look up a value in an @INISection@.
lookupSValue :: INIKey -> INISection -> Maybe [INIVal]
lookupSValue = M.lookup


-- | Look up a value in an @INIFile@.
lookupValue :: INIKey -> INISectName -> INIFile -> Maybe [INIVal]
lookupValue key sect file = M.lookup sect file >>= M.lookup key


-- **** PARSER ****

-- | Parse an INI file into an @INIFile@.
--
-- An INI file comprises a sequence of sections.
--
-- A section starts with a header, which declares the name of the section or subsection.
-- The header is followed by a sequence of key-value declarations.
--
-- Whitespace between and inside sections is ignored, as are comment lines, which
-- begin with @#@ or @;@.
parseIniFile :: B.ByteString -> Either String INIFile
parseIniFile inp = res
  where
    inpLines = C.lines inp
    -- Remove leading and trailing white space
    noLead = map (C.dropWhile (== ' ')) inpLines
    noTrail = map (C.takeWhile (/= ' ')) noLead
    -- Remove comments and blank lines
    noComments = filter ((`C.notElem` "#;") . C.head) noTrail
    noBlank = filter (not . C.null) noComments
    parseRes = parse parseIni $ C.unlines noBlank
    res = case parseRes of
      ErrorResult e -> Left $ show e
      Result _ out -> Right out

parseIni :: Parser INIFile
parseIni = undefined
-- Top/main alg:
-- Split into lines
-- Remove leading and trailing white space
-- Join lines ending with '\' with the next line (or handle in value parser?)
-- Map a general parser over the lines?
-- Gather the results into maps

-- Your implementation goes here.
--
-- parseIniFile should return @Left errmsg@ on error,
-- or @Right parsedResult@ on success.

-- Only alphanumeric characters, - and . are allowed in section names.
-- TODO Subsection names are case sensitive and can contain any characters
-- except newline (double quote " and backslash have to be escaped
-- as \" and \\, respectively).
parseSectionName :: Parser INISectName
parseSectionName = betweenChars '[' ']' $ sect ||| subSect
  where
    sectName = list1 $ alphaNum ||| is '-' ||| is '.'
    sect = fmap (ISect . C.map toLower) sectName
    subSect = do
      sect' <- sectName
      _ <- is ' '
      subSect' <- betweenChars '"' '"' $ list1 $ noneOf "\n"
      return $ ISubsect (C.map toLower sect') subSect'


-- | The absence of a value should be interpreted as 'True'
parseVar :: Parser (INIKey, INIVal)
parseVar = do
  key <- parseKey
  val <- is '=' >>> parseVal ||| valueParser (IBool True)
  return (key, val)


 -- The variable names are case-insensitive, allow only alphanumeric characters
 -- and '-', and must start with an alphabetic character.
parseKey :: Parser INIKey
parseKey = do
  c <- satisfy isAlpha
  cs <- list (alphaNum ||| is '-')
  return $ C.cons (toLower c) (C.map toLower cs)


-- TODO: Handle:
-- case insensitivity.
-- leading and trailing white space
-- new line continuations
parseVal :: Parser INIVal
parseVal = parseBool ||| parseInt ||| parseString
  where
    parseBool = parseTrue ||| parseFalse
    parseTrue = (isStr "on" ||| isStr "true" ||| isStr "yes") >>> valueParser (IBool True)
    parseFalse = (isStr "off" ||| isStr "false" ||| isStr "no") >>> valueParser (IBool False)

-- String values may be entirely or partially enclosed in double quotes.
-- You need to enclose variable values in double quotes if you want to
-- preserve leading or trailing whitespace, or if the variable value
-- contains comment characters (i.e. it contains # or ;).
-- Double quote " and backslash \ characters in variable values must
-- be escaped: use \" for " and \\ for \.
-- The following escape sequences (beside \" and \\) are recognized: \n for newline character (NL), \t for horizontal tabulation (HT, TAB) and \b for backspace (BS). Other char escape sequences (including octal escape sequences) are invalid.
-- Variable values ending in a \ are continued on the next line in the customary UNIX fashion.
parseString :: Parser INIVal
parseString = fmap IString $ list1 (noneOf "#;") ||| betweenChars '"' '"' (list1 charParser)


-- Int: Integers should have an optional sign (+ or -) followed by one or more base-10 digits.
-- Your parser should accept optional suffixes k, M, G, T, P, and E, indicating that the
-- value should be scaled by 2^10, 2^20, etc.
parseInt :: Parser INIVal
parseInt = do
  -- readInteger handles optional + or -
  base <- fmap (fst . fromJust . C.readInteger) $ list1 $ satisfy isNumber
  scale <- (is 'k' >>> valueParser (2^10))
           ||| (is 'M' >>> valueParser (2^20))
           ||| (is 'G' >>> valueParser (2^30))
           ||| (is 'T' >>> valueParser (2^40))
           ||| (is 'P' >>> valueParser (2^50))
           ||| (is 'E' >>> valueParser (2^60))
           ||| valueParser 1
  fmap IInt $ return $ base * scale
