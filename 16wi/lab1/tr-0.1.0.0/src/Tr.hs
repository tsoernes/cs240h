-- | Haskell tr implementation. Just supports the swap and delete modes:
-- * tr string1 string2
-- * tr -d string1
--
-- PLEASE DON'T CHANGE THE INTERFACE OF THIS FILE AS WE WILL EXPECT IT TO BE
-- THE SAME WHEN TESTING!
module Tr where

import Data.List (find)

-- | Just to give `tr` a more descriptive type
type CharSet = String

-- | 'tr' - the characters in the first argument are translated into characters
-- in the second argument, where first character in the first CharSet is mapped
-- to the first character in the second CharSet.
--
-- If the second CharSet is a `Nothing` value, then 'tr' should run in delete
-- mode where any characters in the input string that match in the first
-- CharSet should be removed.
--
-- The third argument is the string to be translated (i.e., STDIN) and the
-- return type is the output / translated-string (i.e., STDOUT).
--
-- translate mode: tr "eo" (Just "oe") "hello" -> "holle"
-- delete mode: tr "e" Nothing "hello" -> "hllo"
--
-- It's up to you how to handle the first argument being the empty string, or
-- the second argument being `Just ""`, we will not be testing this edge case.
tr :: CharSet -> Maybe CharSet -> String -> String
tr _inset _outset xs = case _outset of
  Nothing -> trDelete _inset xs
  Just outset -> trReplace _inset outset xs


trDelete :: CharSet -> String -> String
trDelete inset xs = filter (`notElem` inset) xs


-- If the first CharSet is longer
-- than the second CharSet, the last character found in the second CharSet is
-- duplicated until it matches in length.
-- If the first CharSet is shorter than the second, we just truncate
-- the second CharSet to be the same length as the first.
trReplace :: CharSet -> CharSet -> String -> String
trReplace inset outset xs = map f xs
  where
    f c = case find (\(inC', _) -> inC' == c) subs of
            Just (_, outC) -> outC
            Nothing -> c
    subs = zip inset outset'
    l1 = length inset
    l2 = length outset
    outset' = if l1 > l2
      then outset ++ (replicate (l1-l2) $ last outset)
      else take l1 outset

