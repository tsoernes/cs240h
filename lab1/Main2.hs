{-# OPTIONS_GHC -Wno-unused-binds #-}

module Main (main, matchGlob) where

import Data.Char (chr, ord)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Char(isUpper)

main :: IO()
main = undefined

specials :: String
specials = "?*\\["

type GlobPattern = String

-- | matchGlob
-- >>> matchGlob "abcde" "abcde"
-- True
-- >>> matchGlob "abcde" "abcd"
-- False
-- >>> matchGlob "a]b" "a]b"
-- True
-- >>> matchGlob "a[b" "a[b"
-- False
-- >>> matchGlob "\a\b\c\d\e" "abcde"
-- True
-- >>> matchGlob "-adf]ai1" "-adf]ai1"
-- True
-- >>> matchGlob "\[a]" "[a]"
-- True
-- >>> matchGlob "\*\*\?" "**?"
-- True
-- >>> matchGlob "\\a\\" "\a\"
-- True
-- >>> matchGlob "ab\*ba" "ab*ba"
-- True
-- >>> matchGlob "ab\[ba" "ab[ba"
-- True
-- >>> matchGlob "ab[a\]]ba" "ab]ba"
-- True
-- >>> matchGlob "ab[a\]]ba" "ababa"
-- True
-- >>> matchGlob "[ab[c]" "c"
-- True
-- >>> matchGlob "[ab[c]" "["
-- True
-- >>> matchGlob "[a-z]" "b"
-- True
-- >>> matchGlob "[a-z]" "0"
-- False
-- >>> matchGlob "[---]" "-"
-- True
-- >>> matchGlob "[a-c-z]" "b"
-- True
-- >>> matchGlob "[a-c-z]" "d"
-- False
-- >>> matchGlob "[a-c-z]" "-"
-- True
-- >>> matchGlob "[a-c-z]" "z"
-- True
-- >>> matchGlob "[z-a]" "a"
-- False
-- >>> matchGlob "[abc-]" "-"
-- True
-- >>> matchGlob "[abc-]" "c"
-- True
matchGlob :: GlobPattern -> String -> Bool
matchGlob _ _ = False

type Input = String
data ParseError = Failed deriving (Eq, Show)
data ParseResult a = ErrorResult ParseError | Result Input a deriving Eq

instance Show a => Show (ParseResult a) where
  show (ErrorResult e) = show e
  show (Result i a) = "Result >" ++ i ++ "< " ++ show a

showRes :: Show a => ParseResult a -> String
showRes (ErrorResult e) = show e
showRes (Result _ a) = show a

newtype Parser a = P {
  parse :: Input -> ParseResult a
}

-- | Consume no input, succeed with given value
valueParser :: a -> Parser a
valueParser out = P (`Result` out)

failed :: Parser a
failed = P (\_ -> ErrorResult Failed)

-- | Consume a character, fail if input is empty
charParser :: Parser Char
charParser = P (\inp -> case inp of
                         [] -> ErrorResult Failed
                         (c:cs) -> Result cs c)


-- | Consume and discard input from first parser, continue with second parser
(>>>) :: Parser a -> Parser b -> Parser b
pa >>> pb = pa >>= const pb

-- | Try the first parser, and if it fails, try the second
(|||) :: Parser a -> Parser a -> Parser a
(|||) p1 p2 = P (\inp -> case parse p1 inp of
                           ErrorResult _ -> parse p2 inp
                           Result i a -> Result i a)

infixl 3 |||

-- | Parse 0 or more values
list :: Parser a -> Parser [a]
list pa = list1 pa ||| valueParser []

-- | Parse 1 or more values
list1 :: Parser a -> Parser [a]
list1 pa = pa >>= (\a -> list pa >>= (\as -> valueParser $ a:as))

-- | Parse 0 or more strings, concatenate result
concatLi :: Parser String -> Parser String
concatLi pa = concatLi1 pa ||| valueParser []

-- | Parse 1 or more strings, concatenate result
concatLi1 :: Parser String -> Parser String
concatLi1 pa = list1 pa >>= (valueParser . concat)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = charParser >>= (\a -> if p a then valueParser a else failed)

is :: Char -> Parser Char
is c = satisfy (==c)

oneOf :: String -> Parser Char
oneOf str = satisfy (`elem` str)

noneOf :: String -> Parser Char
noneOf str = satisfy (`notElem` str)

betweenChars :: Char -> Char -> Parser a -> Parser a
betweenChars l r pa = is l *> pa <* is r

parseLit :: Parser Char
parseLit = noneOf specials

-- | Parser for "?" pattern
parseWildcard1 :: Parser Char
parseWildcard1 = charParser

-- | Parser for "*" pattern
parseWildcard :: Parser String
parseWildcard = list charParser

-- Note that we don't treat ] as a special character unless we are trying to close a set match.
-- | Parse set pattern
parseSet :: Parser String
parseSet = betweenChars '[' ']' $ concatLi parsePtrnNoSet

parsePtrnNoSet :: Parser String
parsePtrnNoSet = undefined

parsePtrn :: Parser String
parsePtrn = parseSet ||| failed

parseRange :: String -> Parser String
parseRange ptrn = undefined -- ord chr

instance Functor Parser where
  --fmap :: (a -> b) -> Parser a -> Parser b
  fmap f pa = pa >>= (valueParser . f)

instance Applicative Parser where
  pure = valueParser
  --(<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) pf pa = pf >>= (\f -> pa >>= (valueParser . f))

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) pa f = flBindP f pa

flBindP :: (a -> Parser b) -> Parser a -> Parser b
flBindP f pa = P (\inp -> case parse pa inp of
                          ErrorResult e -> ErrorResult e
                          Result i a -> parse (f a) i)


