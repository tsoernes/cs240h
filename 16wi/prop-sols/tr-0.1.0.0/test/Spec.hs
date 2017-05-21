-- | Test Haskell tr implementation.
module Main (main) where

import Control.Arrow
import Data.List
import Test.Hspec
import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Property (Result, rejected, liftBool)

import Tr

type CharSet' = NonEmptyList Char

trS :: CharSet -> CharSet -> String -> String
trS inset outset = tr inset (Just outset)

trD :: CharSet -> String -> String
trD inset = tr inset Nothing

deDup :: CharSet' -> Maybe CharSet
deDup (NonEmpty xs) | xs' <- nub xs, not (null xs') = Just xs'
                    | otherwise                     = Nothing

main :: IO ()
main = hspec $ do
  -- edgeCases
  translateTests
  deleteTests

-- | Not testing students on these...
edgeCases :: Spec
edgeCases = describe "empty edge-cases" $ do
  it "tr '' ''" $
    tr "" (Just "") "a" `shouldBe` "a"
  it "tr '' 'a'" $
    tr "" (Just "a") "a" `shouldBe` "a"
  it "tr 'a' ''" $
    tr "a" (Just "") "a" `shouldBe` "a"
  it "tr '' Nothing" $
    tr "" Nothing "a" `shouldBe` "a"

-- | Translate mode tests
translateTests :: Spec
translateTests = describe "translate mode" $ do
  describe "single translate" $ do
    it "a -> a" $
      trS "a" "a" "a" `shouldBe` "a"
    it "a -> b" $
      trS "a" "b" "a" `shouldBe` "b"
    it "a -> ." $
      trS "a" "." "a" `shouldBe` "."
    it "a -> ' '" $
      trS "a" " " "a" `shouldBe` " "
    it "empty input" $
      trS "a" "b" "" `shouldBe` ""
    it "no match 1" $
      trS "a" "b" "c" `shouldBe` "c"
    it "no match 2" $
      trS "a" "b" "b" `shouldBe` "b"

  describe "dont parse arguments again" $ do
    it "-d -> ef" $
      trS "-d" "ef" "abc-def" `shouldBe` "abcefef"
    it "ef -> -d" $
      trS "ef" "-d" "abc-def" `shouldBe` "abc-d-d"

  describe "stream translate" $ do
    it "a -> a" $
      trS "a" "a" "aaaa" `shouldBe` "aaaa"
    it "a -> b" $
      trS "a" "b" "aaaa" `shouldBe` "bbbb"
    it "ab -> ba" $
      trS "ab" "ba" "abba" `shouldBe` "baab"
    it "abc -> bac" $
      trS "abc" "bac" "cabcbac" `shouldBe` "cbacabc"
    it "abc -> bca" $
      trS "abc" "bca" "abccba" `shouldBe` "bcaacb"
    it "no match" $
      trS "abc" "cba" "defghijkl" `shouldBe` "defghijkl"
    it "big stream" $
      let bigString = concat $ replicate 100000 "bce"
          outString = concat $ replicate 100000 "cdf"
      in trS ['a'..'z'] (['b'..'z'] ++ ['a']) bigString `shouldBe` outString

  describe "extend input set" $ do
    it "abc -> d" $
      trS "abc" "d" "abcd" `shouldBe` "dddd"
    it "abcd -> de" $
      trS "abcd" "de" "abcd" `shouldBe` "deee"

  describe "quick-check tests" $ do
    it "empty input is identity" $ property pEmptyID
    it "tr x x = id"             $ property pTrAsAsID
    it "tr xs 'a' removes xs"    $ property pRemove
    it "tr preserves length"     $ property pLength
    it "tr quasi invertible"     $ property pQuasiInvertible

  where
    pEmptyID :: CharSet' -> CharSet' -> Result
    pEmptyID set1 (NonEmpty set2)
      | Just set1' <- deDup set1
      = liftBool $ trS set1' set2 "" == ""
      | otherwise
      = rejected

    pTrAsAsID :: CharSet' -> String -> Result
    pTrAsAsID set1 input
      | Just set1' <- deDup set1
      = liftBool $ trS set1' set1' input == input
      | otherwise
      = rejected

    pRemove :: CharSet' -> String -> Result
    pRemove set1 input
      | Just set1' <- deDup set1, 'z' `notElem` set1'
      = let out = trS set1' "z" input
        in liftBool $ null $ intersect out set1'
      | otherwise
      = rejected

    pLength :: CharSet' -> CharSet' -> String -> Result
    pLength set1 set2 input
      | Just set1' <- deDup set1, Just set2' <- deDup set2
      = liftBool $ length input == length (trS set1' set2' input)
      | otherwise
      = rejected

    pQuasiInvertible :: CharSet' -> CharSet' -> String -> Result
    pQuasiInvertible set1 set2 input
      | Just set1' <- deDup set1, Just set2' <- deDup set2
      = let -- establish preconditions
            sz = min (length set1') (length set2')
            xs = take sz set1'
            ys = take sz set2'

            -- map back and forth
            f  = trS xs ys input
            g  = trS ys xs f
            f' = trS xs ys g
            g' = trS ys xs f'
        in liftBool $ g == g'

      | otherwise
      = rejected

-- | Delete mode tests
deleteTests :: Spec
deleteTests = describe "delete mode" $ do
  describe "empty string is empty out" $ do
    it "empty 1" $ trD "x" "" `shouldBe` ""
    it "empty 2" $ trD "y" "" `shouldBe` ""

  describe "single delete" $ do
    it "." $
      trD "." "a.b.c.d." `shouldBe` "abcd"
    it "a" $
      trD "a" "a.b.c.d." `shouldBe` ".b.c.d."
    it "delete all (single char)" $
      trD "x" "x" `shouldBe` ""
    it "delete all (multi char)" $
      trD "x" "xxxxx" `shouldBe` ""

  describe "multi delete" $ do
    it ".-" $
      trD ".-" "a.b.c-.d-.e" `shouldBe` "abcde"
    it "-." $
      trD "-." "a.b.c-.d-.e" `shouldBe` "abcde"
    it "prefix" $
      trD "_" "_____abc" `shouldBe` "abc"
    it "postfix" $
      trD "_"  "abc____" `shouldBe` "abc"

  describe "quick-check tests" $ do
    it "empty input is identity"        $ property pEmptyID
    it "delete removes all occurrences" $ property pRemoveAll
    it "delete idempotent"              $ property pIdempotent
    it "delete doesn't over-remove"     $ property pNoOverRemove

  where
    pEmptyID :: CharSet' -> Result
    pEmptyID set1
      | Just set1' <- deDup set1
      = liftBool $ trD set1' "" == ""
      | otherwise
      = rejected

    pRemoveAll :: CharSet' -> String -> Result
    pRemoveAll set1 input
      | Just set1' <- deDup set1
      = liftBool $ null $ intersect set1' $ trD set1' input
      | otherwise
      = rejected

    pIdempotent :: CharSet' -> String -> Result
    pIdempotent set1 input
      | Just set1' <- deDup set1
      = let out1 = trD set1' input
            out2 = trD set1' out1
        in liftBool $ out1 == out2
      | otherwise
      = rejected

    pNoOverRemove :: CharSet' -> String -> Result
    pNoOverRemove set1 input
      | Just set1' <- deDup set1, notRm <- ['a'..'z'] \\ set1', not (null notRm)
      = let ouput   = trD set1' input
            count   = map (head &&& length) . group . sort
            preC    = count input
            posC    = count ouput
            check a = lookup a preC == lookup a posC
        in liftBool $ all check notRm
      | otherwise
      = rejected

