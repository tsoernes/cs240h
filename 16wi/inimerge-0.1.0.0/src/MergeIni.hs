module MergeIni (
    mergeINI
  ) where

-- import qualified Data.Map.Strict as Map
-- import Data.Map.Strict (Map)

import ParseIni

-- | Merge two INI files given a common ancestor.
mergeINI :: INIFile             -- ^ Our version of the file (HEAD)
         -> INIFile             -- ^ The merge base (common ancestor)
         -> INIFile             -- ^ The version we are merging in
         -> INIFile
mergeINI = undefined -- TODO: Implement!

