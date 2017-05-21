{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GenIni (
  -- * quickCheckable properties
  prop_parseRaw, prop_printParse
  -- * Generators for random Ini Files
  , Ini(..), RawIni(..)
  , arbRawIni, arbINIFile
  ) where

import Control.Monad
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as L
import qualified Data.ByteString.Char8 as S8
import Data.Char
import qualified Data.Map as Map
import Data.Monoid
import Test.QuickCheck

-- import Gpg
import ParseIni
import PrettyPrintIni

type Builder = L.Builder

ceilSqrt :: Int -> Int
ceilSqrt = ceiling . (sqrt :: Double -> Double) . fromIntegral

capSize :: Int -> Gen a -> Gen a
capSize = scale . max

arbAscii :: Gen Char
arbAscii = frequency [ (100, choose (' ', '~'))
                     , (2, elements ['\t', '\n'])
                     , (1, choose ('\0', '\127')) ]

-- | Generate an arbitrary string for a subsection name or value
arbString :: Bool               -- ^ The string can contain newlines
          -> Bool               -- ^ The string can contain comment characters
          -> Gen Builder
arbString nlok comok =
  L.byteString . S8.pack . concat <$> listOf char
  where char :: Gen String
        char = do
          c <- arbAscii
          case c of
            '\n' | nlok -> elements ["\\\n", "\\n"]
                 | otherwise -> char
            '\\' | nlok -> return "\\\\"
                 | otherwise -> (\c' -> ['\\', c']) <$>
                                frequency [(1, return '\\'),
                                           (3, arbAscii `suchThat` (/= '\n')) ]
            '\b' -> elements ["\b", "\\b"]
            '\t' -> elements ["\t", "\\t"]
            '"' -> return "\\\""
            _ | not comok && c `elem` (";#"::String) -> char
              | otherwise -> return [c]

arbWsChar :: Gen Char
arbWsChar = frequency [(2, return ' '), (1, return '\t')]

arbWs :: Bool -> Gen String
arbWs nonEmpty = frequency [(2, fmap (: []) arbWsChar),
                            (1, listOf1 arbWsChar),
                            (if nonEmpty then 0 else 2, return "")]

arbSectName :: Gen Builder
arbSectName = oneof [sec, subsec]
  where secname = listOf1 (arbAscii `suchThat` isTagChar)
        sec = do sn <- secname
                 return $ L.string7 $ '[' : sn ++ "]"
        subsec = do sn <- secname
                    ws <- arbWs True
                    let prefix = L.string7 $ '[' : sn ++ ws ++ "\""
                    ss <- arbString False True
                    return $ prefix <> ss <> "\"]"

arbComment :: Gen Builder
arbComment = capSize 20 $ do
  ws <- arbWs False
  c <- elements "#;"
  contents <- listOf (arbAscii `suchThat` (/= '\n'))
  return $ L.string7 $ ws ++ c : contents

arbKey :: Gen String
arbKey = do
  k0 <- arbAscii `suchThat` isAlpha
  (k0:) <$> capSize 9 (listOf $ arbAscii `suchThat` isTagChar)

arbKV :: Gen Builder
arbKV = capSize 40 $ do
  ws0 <- arbWs False
  k <- arbKey
  ws1 <- arbWs False
  val <- frequency [(3, arbVal),
                    (2, liftM2 mappend arbVal arbComment),
                    (1, return "")]
  return $ L.string7 (concat [ws0, k, ws1]) <> val <> "\n"
  where arbVal :: Gen Builder
        arbVal = do
          eq <- L.string7 . ('=':) <$> arbWs False
          n <- choose (0, 4)
          v <- vectorOf n subStr
          return $ mconcat $ eq : v
        subStr = oneof [ arbString True False,
                         quote <$> arbString True True ]
        quote s = q <> s <> q
        q = L.char7 '"'

arbBlank :: Gen Builder
arbBlank = mconcat <$> capSize 5 (listOf $ oneof [blank, comment])
  where blank = L.string7 . (++ "\n") <$> arbWs False
        comment = (<> "\n") <$> arbComment

arbSect :: Gen Builder
arbSect = do
  name <- arbSectName
  contents <- scale ceilSqrt $ mconcat <$> listOf (oneof [arbKV, arbBlank])
  return $ name <> contents

-- | Generate a raw ByteString that should be parseable as an Ini
-- file.  The output may contain comments, weird indentation,
-- continuations lines, useless sections with no key-value pairs,
-- weird capitalization, etc.
arbRawIni :: Gen S.ByteString
arbRawIni = do
  b <- arbBlank
  ss <- listOf arbSect
  return $ toByteString $ mconcat (b : ss)

-- | Newtype wrapper around 'arbRawIni'.
newtype RawIni = RawIni { unRawIni :: S.ByteString }
instance Show RawIni where show (RawIni bs) = show bs
instance Arbitrary RawIni where
  arbitrary = RawIni <$> arbRawIni


arbINISectName :: Gen INISectName
arbINISectName =
  liftM2 toSectName (listOf1 $ arbitrary `suchThat` isTagChar) $
  oneof [ return Nothing
        , Just <$> listOf (arbAscii `suchThat` (/= '\n')) ]

instance Arbitrary INISectName where
  arbitrary = arbINISectName


arbINISection :: Gen INISection
arbINISection =
  scale ceilSqrt $ fmap Map.fromList $ listOf1 $
  liftM2 (,) (toKey <$> arbKey) (capSize 20 $ listOf1 arbBS)
  where arbBS = S8.pack <$> listOf arbAscii

-- | Generate a random INIFile structure.  Uses 'toSectName' and
-- 'toKey', which should take care of normalizing case it tags.
arbINIFile :: Gen INIFile
arbINIFile =
  fmap Map.fromList $ listOf $ liftM2 (,) arbINISectName arbINISection

-- | Newtype wrapper around 'arbINIFile'.
newtype Ini = Ini { unIni :: INIFile }
instance Show Ini where show (Ini ini) = show ini
instance Arbitrary Ini where
  arbitrary = Ini <$> arbINIFile
  shrink (Ini ini) =
    map (Ini . Map.fromList . filter (\(_, kvs) -> not $ Map.null kvs)) $
    shrinkList shrinkSec (Map.toList ini)
    where shrinkSec (name, contents) =
            map (\m -> (name, Map.fromList m)) $ sublists $ Map.toList contents
          sublists [] = []
          sublists (x:xs) = xs:[x:xs' | xs' <- sublists xs]


{-
-- | Call out to git to have it parse an INI file.
gitListIni :: S.ByteString -> IO (Maybe S.ByteString)
gitListIni input =
  (Just <$> readProcessBS "git" ["config", "-l", "-f", "-"] input)
  `catch` \(BadExit _ _) -> return Nothing

-- | This was a nice idea, except that for blank keys (without =), git
-- doesn't print the trailing = sign, yet we no longer have this
-- information after parsing.  (Also, git sorts differently, but that
-- is easy to fix")
prop_gitAgree :: RawIni -> Property
prop_gitAgree (RawIni raw) = ioProperty $ do
  gitOut <- gitListIni raw
  let listOut = listIni <$> tryParse parseIni raw
  return $ gitOut == listOut
-}

-- | Check that you can parse an raw INI file, and that printing,
-- reparsing, and reprinting gives the same result.
prop_parseRaw :: RawIni -> Property
prop_parseRaw (RawIni raw) =
  let Right ini1 = parseIniFile raw
      print1 = prettyPrint ini1
      Right ini2 = parseIniFile print1
      print2 = prettyPrint ini2
  in ini1 === ini2 .&&. print1 === print2

-- | Check that printing then reparsing a random 'INIFile' returns the
-- same result as the original.
prop_printParse :: Ini -> Property
prop_printParse (Ini ini1) =
  let print1 = prettyPrint ini1
      Right ini2 = parseIniFile print1
  in ini1 === ini2
