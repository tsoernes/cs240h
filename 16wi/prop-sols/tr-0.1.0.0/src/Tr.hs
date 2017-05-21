-- | Haskell tr implementation. Just supports the swap and delete modes:
-- * tr string1 string2
-- * tr -d string1
--
-- PLEASE DON'T CHANGE THE INTERFACE OF THIS FILE AS WE WILL EXPECT IT TO BE
-- THE SAME WHEN TESTING!
module Tr
    ( CharSet
    , tr
    ) where

import Data.List

-- | Just to give `tr` a more descriptive type
type CharSet = String

-- | 'tr' - the characters in the first argument are translated into characters
-- in the second argument, where first character in the first CharSet is mapped
-- to the first character in the second CharSet. If the first CharSet is longer
-- than the second CharSet, the last character found in the second CharSet is
-- duplicated until it matches in length.
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
tr ""     _            xs = xs
tr _     (Just "")     xs = xs
tr inset (Just outset) xs = trS inset outset xs
tr inset Nothing xs       = trD inset xs

-- | tr swap mode
trS :: CharSet -> CharSet -> String -> String

-- inset > outset
trS inset outset xs
  | length inset > length outset
  = let lenDiff = length inset - length outset
        extension = replicate lenDiff $ last outset
        outset' = outset ++ extension
    in trS inset outset' xs

-- inset <= outset
trS inset outset xs = map swap xs
  where
    swap x | Just i <- x `elemIndex` inset = outset !! i
           | otherwise = x

-- Alternative 1:
-- -- inset <= outset (without map, own recursion)
-- trS inset outset xs' = tr' xs'
--   where
--     tr' (x:xs) | Just i <- x `elemIndex` inset
--       = let outc = outset !! i
--         in outc : tr' xs
--     tr' (x:xs) = x : tr' xs
--     tr' []     = []

-- Alternative 2:
-- inset <= outset (for fun, mind-expanding, not great solution)
-- trS inset outset xs = map replace xs
--   where swap i o (Right c) | i == c = Left o
--         swap _ _ c = c
--
--         cSwaps = zipWith swap inset outset
--
--         fromEither (Right x) = x
--         fromEither (Left x) = x
--
--         replace = fromEither . foldl1 (.) cSwaps . Right

-- | tr delete mode
trD :: CharSet -> String -> String
trD inset = filter (`notElem` inset)

-- Alternative 1:
-- -- | tr delete mode (without filter, own recursion)
-- trD :: CharSet -> String -> String
-- trD inset = tr'
--   where
--     tr' []                      = []
--     tr' (x:xs) | x `elem` inset = tr' xs
--     tr' (x:xs)                  = x : tr' xs

