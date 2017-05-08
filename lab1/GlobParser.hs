-- {-# OPTIONS_GHC -Wno-unused-binds #-}

module GlobParser (globParser) where

import Data.Char (ord)

import Parser

specials :: String
specials = "?*\\["

-- It does not make sense to treat '?' or '*' as special inside a set
--   because that would defeat the purpose of the set. Sets cannot be
--   recursive so '[' is not a special char either.
--   '-' is not a special symbol by itself because it requires a char on either side.
setSpecials :: String
setSpecials = "\\]"

-- Make sure that both parsers and inpParser consume their input
--
-- The parser that's used on the GlobPattern must produce
-- a Parser String which is then used on the input to check
-- that 1) the input is fully consumed by the parser
--      2) the parser doesn't fail
--      3) the output corresponds to the input
--
-- The GlobParser chains together 0 or more pattern parsers,
-- where the latter parses a particular pattern, e.g. set or wildcard.

type PatternParser = Parser (Parser String)

globParser :: PatternParser
globParser = parsePtrns

-- | Parse a literal, including given special characters (e.g. "\*")
parseLits :: String -> PatternParser
parseLits specials' = do
     c <- (is '\\' >>> charParser) ||| noneOf specials'
     valueInpParser $ is c

-- | Parser for "?" pattern
parseWildcard1 :: PatternParser
parseWildcard1 = is '?' >>> valueInpParser charParser

-- | Parser for "*" pattern
parseWildcard :: PatternParser
parseWildcard = do
  _ <- is '*'
  -- Attempts to parse pattern after '*' first, and if that
  -- fails, parses one character then recursively try again.
  -- If '*' is last, then just consume any and all characters.
  ptrnParsers <- list parsePtrn
  let p = if null ptrnParsers
      then list charParser
      else sequenceParserConcat ptrnParsers ||| consParsers charParser p
  return p

-- | Parse set pattern
parseSet :: PatternParser
parseSet = betweenChars '[' ']' $ anyParser <$> list parsePtrnInSet

-- | Parse any single pattern inside a set
parsePtrnInSet :: PatternParser
parsePtrnInSet = parseRange ||| parseLits setSpecials

-- | Parse any single pattern
parsePtrn :: PatternParser
parsePtrn = parseSet
           ||| parseRange
           ||| parseWildcard
           ||| parseWildcard1
           ||| parseLits specials

-- | Parse zero or more patterns
parsePtrns :: PatternParser
parsePtrns = sequenceParserConcat <$> list parsePtrn

-- | Parse 1 character within a detected range, e.g. "a-k"
parseRange :: PatternParser
parseRange = do
  l <- noneOf $ specials ++ setSpecials
  _ <- is '-'
  r <- noneOf $ specials ++ setSpecials
  return $ inpParseRange l r

inpParseRange :: Char -> Char -> Parser String
inpParseRange l r = charParser >>= \c ->
                    if (ord c >= ord l) && (ord c <= ord r)
                       then valueParserS c
                       else failed

