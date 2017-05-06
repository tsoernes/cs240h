{-# OPTIONS_GHC -Wno-unused-binds #-}

module GlobParser where

import Data.Char (ord)

import Parser

specials :: String
specials = "?*\\["

-- It does not make sense to treat '?' or '*' as special inside a set
--   because that would defeat the purpose of the set. Sets cannot be
--   recursive so '[' is not a special char either.
setSpecials :: String
setSpecials = "\\-]"

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
     c <- is '\\' >>> charParser ||| noneOf specials'
     valueInpParser $ is c

-- | Parser for "?" pattern
parseWildcard1 :: PatternParser
parseWildcard1 = is '?' >>> valueInpParser charParser

-- | Parser for "*" pattern
parseWildcard :: PatternParser
parseWildcard = do
  _ <- is '*'
  -- Attempts to parse pattern after "*" first, and if
  -- that fails, parses one character then recurse.
  ptrnParser <- parsePtrns
  let p = ptrnParser ||| consParsers charParser p
  return p

-- | Parse set pattern
--
-- how to handle multiple patterns within a set? e.g. [ab-dk]
-- should produce: parseLit a OR parseRange b-d OR parseLit k
-- actually produces: parseLit a THEN parseRange b-d THEN parseLit k
-- should [abc] not match on "abc"; just "a", "b" and "c"
parseSet :: PatternParser
parseSet = betweenChars '[' ']' $ anyParser <$> list1 parsePtrnInSet

-- | Parse any single pattern inside a set
parsePtrnInSet :: PatternParser
parsePtrnInSet = parseRange ||| parseLits setSpecials

-- | Parse at least one pattern inside a set
parsePtrnsInSet1 :: PatternParser
parsePtrnsInSet1 = sequenceParserConcat <$> list1 parsePtrnInSet

-- | Parse any single pattern
parsePtrn :: PatternParser
parsePtrn = parseSet
           ||| parseRange
           ||| parseWildcard
           ||| parseWildcard1
           ||| parseLits specials

-- | Parse 1 or more patterns
parsePtrns :: PatternParser
parsePtrns = sequenceParserConcat <$> list parsePtrn

-- | Parse 1 character within a detected range, e.g. "a-k"
parseRange :: PatternParser
parseRange = do
  l <- charParser
  _ <- is '-'
  r <- charParser
  return $ inpParseRange l r

inpParseRange :: Char -> Char -> Parser String
inpParseRange l r = charParser >>= \c ->
                    if (ord c >= ord l) && (ord c <= ord r)
                       then valueParserS c
                       else failed

