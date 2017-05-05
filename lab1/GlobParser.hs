{-# OPTIONS_GHC -Wno-unused-binds #-}

module GlobParser (
  globParser
) where

import Data.Char (ord)

import Parser

specials :: String
specials = "?*\\["

setSpecials :: String
setSpecials = "-]"

-- The parser that's used on the GlobPattern must produce
-- a Parser String which is then used on the input to check
-- that 1) the input is fully consumed by the parser
--      2) the parser doesn't fail
--      3) the output corresponds to the input
--
-- The first parser is of type GlobParser
-- and the second of type InputParser.
--
-- The GlobParser chains together 0 or more pattern parsers,
-- where the latter parses a particular pattern, e.g. set or wildcard.
--
-- Helper methods for GlobParser are named: glb***Parser
-- Helper methods for StringParser are named: inp***Parser

type StringParser = Parser String
type GlobParser = Parser String
type PatternParser = Parser (Parser Char)

globParser :: GlobParser
globParser = undefined

-- Parse one literal, including special characters (e.g. "\*")
glbParseLits :: PatternParser
glbParseLits = valueParser $ parseSpecial specials

-- Parse one literal inside a set, including special characters (e.g. "\]")
glbParseLitsSet ::PatternParser
glbParseLitsSet = valueParser $ parseSpecial setSpecials

parseSpecial :: String -> Parser Char
parseSpecial str = (is '\\' >>> charParser) ||| noneOf str

-- | Parser for "?" pattern
glbParseWildcard1 :: PatternParser
glbParseWildcard1 = valueParser charParser

-- | Parser for "*" pattern
--    Attempts to parse anything after "*" first, and if
--    that fails, parses one character then recurses.
glbParseWildcard :: Parser (Parser String)
glbParseWildcard = do
  _ <- is '*'
  ptrnParser <- glbParsePtrn
  let p = ptrnParser ||| consParsers charParser p
  return p

-- Note that we don't treat ] as a special character unless we are trying to close a set match.
-- | Parse set pattern
glbParseSet :: Parser String
glbParseSet = undefined --betweenChars '[' ']' $ concatLi parsePtrnNoSet

-- Must handle ranges, escaped chars ... [ac-f\\] either a, c, d, f, \
inpParseSet = undefined
-- data Pattern = Set Pattern | Char Pattern | d
-- Parse any pattern not including sets
glbParsePtrnNoSet :: Parser (Parser String)
glbParsePtrnNoSet = undefined

glbParsePtrn :: Parser (Parser String)
glbParsePtrn = undefined

-- | Parse a character within a range "a-b"
glbParseRange :: PatternParser
glbParseRange = charParser >>= \l ->
                is '-' >>= \_ ->
                charParser >>= \r ->
                  valueParser $ inpParseRange l r

inpParseRange :: Char -> Char -> Parser Char
inpParseRange l r = charParser >>= \c ->
                    if (ord c >= ord l) && (ord c <= ord r)
                       then valueParser c
                       else failed


