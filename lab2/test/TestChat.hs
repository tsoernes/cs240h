-- | Test our chat server.
module Main (main) where

import Test.Hspec
import Test.QuickCheck

import Chat

main :: IO ()
main = hspec $ describe "Testing Lab 2" $ do

  -- example quickcheck test in hspec.
  describe "read" $ do
    it "is inverse to show" $ property $
      \x -> (read . show) x == (x :: Int)

