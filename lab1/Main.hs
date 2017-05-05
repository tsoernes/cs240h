module Main (main, matchGlob) where

--import Parser
import GlobParser
-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Char(isUpper)

main :: IO()
main = undefined

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
matchGlob ptrn inp = inpOK
  where
    stringParser = parse globParser ptrn
    inpOK = case stringParser of
              ErrorResult _ -> False
              Result _ stringParser' -> case parse stringParser' inp of
                                          ErrorResult _ -> False
                                          Result _ inp' -> inp' == inp


type GlobPattern = String
