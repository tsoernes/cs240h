module ParseIni
    ( INISectName(..)
    , INIKey
    , INIVal
    , INISection
    , INIFile
    , parseIniFile
    , toSectName
    , toKey
    , lookupSection
    , lookupValue
    -- * Lower-level functions
    , parseIni
    , parseSection
    , isTagChar
    ) where

import Control.Applicative
import Control.Monad
import Data.Char
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe

import Parser

-- **** TYPES ****
-- These are the types you should use for the results of your parse.
-- Think carefully about what's going on here!

-- |INI files are separated into sections and subsections of key-value pairs.
-- We represent section and subsection identifiers with the INISectName type.
-- Section names are case insensitive strings; subsection names are case sensitive.
data INISectName = ISect    { iSect    :: S.ByteString }
                 | ISubsect { iSect    :: S.ByteString
                            , iSubsect :: S.ByteString }
    deriving (Eq, Ord, Show)

-- |Within each (sub)section, an INI file contains a set of keys and values.
-- Keys are case insensitive strings.
type INIKey = S.ByteString

-- |After parsing key-value pairs, each value should be assigned a type.
-- We represent these types via the @INIVal@ sum type.
type INIVal = S.ByteString

-- |An @INISection@ is a map from @INIKey@s to @INIVal@s.
type INISection = Map INIKey [INIVal]

-- |An @INIFile@ is a map from @INISectName@s to @INISection@s.
type INIFile = Map INISectName INISection


-- **** INTERFACE ****
-- You need to implement these so that we can test your code!
--
-- Why? Because you shouldn't need to expose exactly the way that
-- you handle, e.g., case insensitive string matching in order for
-- someone to use your INI file parser.

-- |Given a section name and possibly a subsection name, return an
-- appropriate @INISectName@. This function accounts for the case
-- insensitivity of the section name.
toSectName :: String -> Maybe String -> INISectName
toSectName sec = maybe (ISect lsec) (ISubsect lsec . S8.pack)
  where lsec = S8.pack $ map toLower sec

-- |Given a key name, return an appropriate @INIKey@. This function
-- accounts for the case insensitivity of the key name.
toKey :: String -> INIKey
toKey [] = error "toKey: invalid null key"
toKey s@(c:_) | not (isAlpha c) = error "toKey: key must start with letter"
              | not (all isTagChar s) = error "toKey: bad character"
              | otherwise = S8.pack $ map toLower s

-- |Look up a section in an @INIFile@.
lookupSection :: INISectName -> INIFile -> Maybe INISection
lookupSection = Map.lookup

-- |Look up a value in an @INISection@.
lookupSValue :: INIKey -> INISection -> Maybe [INIVal]
lookupSValue = Map.lookup

-- |Look up a value in an @INIFile@.
lookupValue :: INIKey -> INISectName -> INIFile -> Maybe [INIVal]
lookupValue k sec ini = Map.lookup sec ini >>= lookupSValue k


-- **** PARSER ****

-- |Parse an INI file into an @INIFile@.
--
-- An INI file comprises a sequence of sections.
--
-- A section starts with a header, which declares the name of the
-- section or subsection.  The header is followed by a sequence of
-- key-value declarations.
--
-- Whitespace between and inside sections is ignored, as are comment
-- lines, which begin with @#@ or @;@.
parseIniFile :: S.ByteString -> Either String INIFile
parseIniFile = either (Left . show) Right . runParser parseIni ""

-- Your implementation goes here.
--
-- parseIniFile should return @Left errmsg@ on error,
-- or @Right parsedResult@ on success.

-- | Returns True for characters that are allowed to appear in section
-- names and keys.  Note that keys additionally must satisfy the
-- requirement that they begin with a letter.  Accepts both upper-case
-- and lower-case letters, even though keys and section names are case
-- insensitive and hence get converted to lower-case by 'toKey'.
isTagChar :: Char -> Bool
isTagChar c = isBetween 'a' 'z' c || isBetween '0' '9' c
              || c == '-' || isBetween 'A' 'Z' c

parseSecTag :: Parser S.ByteString
parseSecTag = S8.map toLower <$> parseWhile1 isTagChar

parseTag :: Parser S.ByteString
parseTag = do
  tag <- parseSecTag
  when (not (isBetween 'a' 'z' (S8.head tag))) mzero
  return tag

parseISubsect :: Parser S.ByteString
parseISubsect = do
  '"' <- parseChar
  dequote [] False
  where dequote out esc = do
          c <- parseChar
          case c of
           _ | esc -> dequote (c:out) False
           '\\'    -> dequote out True
           '"'     -> return $ S8.pack $ reverse out
           _       -> dequote (c:out) False

parseINISectName :: Parser INISectName
parseINISectName = do
  '[' <- parseChar
  sect <- parseSecTag
  result <- (skipWhile1 isHSpace >> ISubsect sect <$> parseISubsect)
            <|> return (ISect sect)
  ']' <- parseChar
  -- skipWhile isHSpace
  return result

-- | Parse a character, but treat end-of-file like a newline
eofToNl :: Parser Char
eofToNl = parseChar <|> pure '\n'

parseINIVal :: Parser S.ByteString
parseINIVal = do
  go [] [] False
  where
    -- If we hit a newline or comment, we must retroactively drop
    -- trailing unquoted whitespace.  So out tracks total parsed
    -- input, and sout tracks what we should return if we hit a
    -- comment.  goEsc parses after a '\\', while go just parses
    -- normally.  q is True when we are inside double quotes.
    goEsc sout out q = do
      c <- eofToNl
      let go' o = go o o q
      case c of
       '\n' -> go sout out q
       'b'  -> go' ('\b':out)
       'n'  -> go' ('\n':out)
       't'  -> go' ('\t':out)
       '\\' -> go' ('\\':out)
       '"'  -> go' ('"':out)
       _    -> mzero
    go sout out q = do
      c <- eofToNl
      case c of
       '\n' | q -> mzero
       '\n' -> finish sout
       '\\' -> goEsc sout out q
       '"'  -> go out out (not q)
       ' ' | not q  -> go sout (' ':out) q
       '\t' | not q -> go sout ('\t':out) q
       ';' | not q  -> skipCommentBody sout
       '#' | not q  -> skipCommentBody sout
       _    -> go (c:out) (c:out) q
    skipCommentBody sout = do
      skipWhile (/= '\n')
      '\n' <- eofToNl
      finish sout
    finish sout = return $ S8.pack $ reverse sout

parseNameValue :: Parser (S.ByteString, S.ByteString)
parseNameValue = do
  name <- parseTag
  skipWhile isHSpace
  let doVal = do
        '=' <- parseChar
        skipWhile isHSpace
        parseINIVal
  let doEmpty = do
        '\n' <- eofToNl
        return S.empty
  value <- doVal <|> doEmpty
  return (name, value)

skipBlank :: Parser ()
skipBlank = do
  skipWhile isHSpace
  void $ maybeParse comment
  nl <- isJust <$> maybeParse (skip (== '\n'))
  when nl skipBlank
  where 
    comment = do
      skip (\c -> c == '#' || c == ';')
      skipWhile (/= '\n')

-- | Parser for the contents of an INI section, i.e., a bunch of name
-- = value pairs (without the @[section]@ header).
parseSection :: Parser INISection
parseSection = do
  nvs <- many (skipBlank *> parseNameValue)
  skipBlank
  return $ foldl (\m (n,v) -> Map.insertWith (flip (++)) n [v] m) Map.empty nvs

-- | Parser for a whole INI file.
parseIni :: Parser INIFile
parseIni = skipBlank >> go Map.empty
  where go ini = maybeParse sectionUpdate >>=
                 maybe (return ini) (\f -> go $ f ini)
        sectionUpdate :: Parser (INIFile -> INIFile)
        sectionUpdate = do
          skipBlank
          s <- parseINISectName
          nvs <- parseSection
          return $ Map.insertWith (Map.unionWith (++)) s nvs
