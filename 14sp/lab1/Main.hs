module Main where

import Parser (Parser(..), ParseResult(..))
import GlobParser (globParser)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Char(isUpper)

main :: IO()
main = undefined

-- | matchGlob
-- >>> matchGlob "abcde" "abcde"
-- True
-- >>> matchGlob "abced" "abcde"
-- False
-- >>> matchGlob "abcde" "abcd"
-- False
-- >>> matchGlob "a]b" "a]b"
-- True
-- >>> matchGlob "a[b" "a[b"
-- False
-- >>> matchGlob "\\a\\b\\c\\d\\e" "abcde"
-- True
-- >>> matchGlob "-adf]ai1" "-adf]ai1"
-- True
-- >>> matchGlob "\\[a]" "[a]"
-- True
-- >>> matchGlob "[]a" "a"
-- False
-- >>> matchGlob "\\*\\*\\?" "**?"
-- True
-- >>> matchGlob "\\\\a\\\\" "\\a\\"
-- True
-- >>> matchGlob "ab\\*ba" "ab*ba"
-- True
-- >>> matchGlob "ab\\[ba" "ab[ba"
-- True
-- >>> matchGlob "ab[a\\]]ba" "ab]ba"
-- True
-- >>> matchGlob "ab[a\\]]ba" "ababa"
-- True
-- >>> matchGlob "[ab[c]" "c"
-- True
-- >>> matchGlob "[abc]" "abc"
-- False
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
-- >>> matchGlob "[a-cf]" "a"
-- True
-- >>> matchGlob "[abc-]" "c"
-- True
-- >>> matchGlob  "*" "adc"
-- True
-- >>> matchGlob  "ac*" "acK_"
-- True
-- >>> matchGlob  "*ac" "\\d*wac"
-- True
-- >>> matchGlob  "a*b" "ab"
-- True
-- >>> matchGlob  "a*b" "axyz"
-- False
-- >>> matchGlob "[abc-]" "c"
-- True
-- >>> matchGlob "[abc-]" "c"
-- True
matchGlob :: GlobPattern -> String -> Bool
matchGlob ptrn inp = inpOK
  where
    stringParser = parse globParser ptrn
    inpOK = case stringParser of
              ErrorResult _ -> False
              Result _ stringParser' -> case parse stringParser' inp of
                                          ErrorResult _ -> False
                                          Result _ inp' -> inp' == inp


printMatchGlob :: Parser (Parser String) -> GlobPattern -> String -> IO()
printMatchGlob p ptrn inp = putStrLn res
  where
    globRes = parse p ptrn
    res = case globRes of
            ErrorResult e1 -> show e1
            Result rem' stringParser -> let stringRes = parse stringParser inp
                                         in "Glob rem: " ++ show rem' ++ "\n" ++ show stringRes

type GlobPattern = String
