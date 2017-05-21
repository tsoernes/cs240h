module MergeIni (
    mergeINI
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import ParseIni

merge3 :: (Ord k, Eq v, Monoid v) =>
          (v -> v -> v -> v)    -- ^ What to do on conflict
       -> Map k v               -- ^ head map
       -> Map k v               -- ^ base map (common ancestor)
       -> Map k v               -- ^ merge_head (what we are merging in)
       -> Map k v
merge3 onConflict ourMap baseMap theirMap =
  Map.fromList $ go (Map.toList ourMap) (Map.toList baseMap)
                    (Map.toList theirMap)
  where
    findVal :: (Ord k, Monoid v) => k -> [(k, v)] -> (v, [(k, v)])
    findVal k ((bk, bv):bs) | bk < k    = findVal k bs
                            | bk == k   = (bv, bs)
    findVal _ b = (mempty, b)

    nextKey ((ka,_):_) ((kb,_):_) = min ka kb
    nextKey ((ka,_):_) []         = ka
    nextKey [] ((kb,_):_)         = kb
    nextKey [] []                 = error "bug in merge3"
    
    go [] _ [] = []
    go ours base theirs | v == mempty = next
                        | otherwise   = (k, v):next
      where k = nextKey ours theirs
            (ourVal, ourNext) = findVal k ours
            (baseVal, baseNext) = findVal k base
            (theirVal, theirNext) = findVal k theirs
            next = go ourNext baseNext theirNext
            v | ourVal == theirVal = ourVal
              | theirVal == baseVal = ourVal
              | ourVal == baseVal = theirVal
              | otherwise = onConflict ourVal baseVal theirVal

-- | Merge two INI files given a common ancestor.
mergeINI :: INIFile             -- ^ Our version of the file (HEAD)
         -> INIFile             -- ^ The merge BASE (common ancestor)
         -> INIFile             -- ^ The version we are merging in (MERGE)
         -> INIFile
mergeINI = merge3 mergeSection

mergeSection :: INISection -> INISection -> INISection -> INISection
mergeSection = merge3 mergeLists

mergeLists :: [INIVal] -> [INIVal] -> [INIVal] -> [INIVal]
mergeLists ours _ theirs = go [] (reverse ours) (reverse theirs)
  where go acc (o:os) (t:ts) | o == t = go (o:acc) os ts
        go acc (o:os) ts              = go (o:acc) os ts
        go acc [] (t:ts)              = go (t:acc) [] ts
        go acc [] []                  = acc

