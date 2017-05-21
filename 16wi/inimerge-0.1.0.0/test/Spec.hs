module Main (main) where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Test.QuickCheck
import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2

import GenIni
import MergeIni

prop_theirsUnchanged :: Ini -> Ini -> Bool
prop_theirsUnchanged (Ini ours) (Ini theirs) =
  ours == mergeINI ours theirs theirs

prop_oursUnchanged :: Ini -> Ini -> Bool
prop_oursUnchanged (Ini ours) (Ini theirs) =
  theirs == mergeINI ours ours theirs

prop_emptyBase :: Ini -> Ini -> Property
prop_emptyBase (Ini ours) (Ini theirs) =
  keys === Map.keys (Map.union ours theirs)
  .&&. all checkSec keys
  where merged = mergeINI ours Map.empty theirs
        keys = Map.keys merged
        checkSec k = Map.keys ms == Map.keys (Map.union os ts)
          where os = fromMaybe Map.empty $ Map.lookup k ours
                ts = fromMaybe Map.empty $ Map.lookup k theirs
                Just ms = Map.lookup k merged

main :: IO ()
main = defaultMain [
    testProperty "Theirs Unchanged" prop_theirsUnchanged
  , testProperty "Ours Unchanged" prop_oursUnchanged
  , testProperty "Empty Base" prop_emptyBase
  ]
