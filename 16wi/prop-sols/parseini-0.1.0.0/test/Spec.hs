{-# Language OverloadedStrings, FlexibleInstances #-}

module Main
    ( main
    ) where

import ParseIni
import PrettyPrintIni

import Data.ByteString.Builder
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Char (toUpper)
import Data.Either (isLeft)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import Data.Monoid
import Test.Hspec
import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Property (failed, liftBool, Result, succeeded)

-- |Run tests on the parser and pretty printer.
main :: IO ()
main = hspec $ describe "Testing parser" $ do
    describe "Testing ParseIni" $
        runTests B.empty (Right M.empty) parseIniFile parseIniFile

    describe "Testing PrettyPrintIni" $
        runTests M.empty B.empty prettyPrint testIdemp

    describe "Running QuickCheck tests" $ do
        it "Correct count for multivalued variables" $ property pMultivalCount
        it "Parser handles random INI files" $ property pRandomIniFile
        it "Pretty Print is idempotent on random INI files" $ property pRandomIdemp




-- **** Top-level test helpers ****

-- |Parameterized test runner.
--
-- This function runs the @allTests@ battery.
runTests eIn eOut eF f = foldr run1 emptyTest allTests
  where run1 (n, i, o) m = (it n $ f i `shouldSatisfy` o) >> m
        emptyTest = it "Empty file" $ eF eIn `shouldBe` eOut

-- |Test the result of a parse by checking that a given value is in the parsed output.
valueIn :: INIVal -> INISectName -> INIKey -> Either String INIFile -> Bool
valueIn v s k r | (Right f) <- r = let value = fromMaybe [] $ lookupValue k s f
                                   in v `elem` value
                | otherwise = False

-- |Test the pretty-printer by checking for idempotence. Returns parsed result.
testIdemp :: B.ByteString -> Either String INIFile
testIdemp s | run1 == run2 = parseRes
            | otherwise = errorRes
  where run1 = prettyPrint <$> parseIniFile s
        run2 = prettyPrint <$> ( parseIniFile =<< run1 )
        parseRes = parseIniFile =<< run2
        errorRes = Left "Pretty printer failed idempotence check."




-- **** QuickCheck tests ****

type MultivalCount = NonNegative Int

-- |Test that, given an INI file with some number of repeated @key=val@ declarations,
-- the resulting parse has that number of values in the @INIFile@.
pMultivalCount :: MultivalCount -> Result
pMultivalCount c | (Left _) <- eParsedOut = failed
                 | otherwise = lengthMatches
  where -- construct input stream that declares a multivalued variable
        (NonNegative cInt) = c
        keyDecls = take cInt $ repeat "key=val\n"
        inFile = B.pack $ "[section]\n" ++ concat keyDecls

        -- parse the generated input stream
        eParsedOut = parseIniFile inFile
        (Right parsedOut) = eParsedOut

        -- extract the key from the parsed output
        sect = toSectName "section" Nothing
        key = toKey "key"
        values = fromMaybe [] $ lookupValue key sect parsedOut

        -- length of values should match value of c
        lengthMatches = liftBool $ cInt == length values

newtype RandomIni = RandomIni B.ByteString
instance Show RandomIni where
    show (RandomIni r) = show r
instance Arbitrary RandomIni where
    arbitrary = RandomIni <$> arbIniFile

-- |Test that a parser is able to handle a randomly generated INI file.
pRandomIniFile :: RandomIni -> Result
pRandomIniFile (RandomIni r) | (Left _) <- pOut = failed
                             | otherwise = succeeded
    where pOut = parseIniFile r

-- |Test that the pretty-printer is idempotent on randomly generated INI files.
pRandomIdemp :: RandomIni -> Result
pRandomIdemp (RandomIni r) | (Left _) <- pOut = failed
                           | otherwise = succeeded
    where pOut = testIdemp r




-- **** HSpec test battery ****
-- rather than supplying tests one at a time, allTests is
-- a list of test specifications that @runTests@ folds over.

-- shorthand for some of our test variables
stringVar = IString "var"
stringVar2 = IString "var2"
stringEmpty = IString ""
keyKey = toKey "key"
secSection = toSectName "section" Nothing
subSectionUC = toSectName "SECTION" $ Just "SUBSECTION"
subSectionLC = toSectName "SectIOn" $ Just "subsection"
subSectionEsc1 = toSectName "SecTion" $ Just "SUBSECTION \"1\""
subSectionEsc2 = toSectName "SecTion" $ Just "SUBSECTION \\2\\"
bTrue = IBool True
bFalse = IBool False

-- test inputs and outputs
allTests = [
  ( "One section"
  , "[section]\nkey=var\nkey2=var2\n"
  , valueIn stringVar secSection keyKey
  )
  ,

  ( "One section, one multi-variable"
  , "[section]\nkey=var\nkey=var2\n"
  , valueIn stringVar2 secSection keyKey
  )
  ,

  ( "Section and subsection"
  , "[section]\nkey=var\n[section \"SUBSECTION\"]\nkey=var2\n"
  , \res -> and $ map ($res) [ valueIn stringVar secSection keyKey
                             , valueIn stringVar2 subSectionUC keyKey ]
  )
  ,

  ( "Multi-valued variable, alternating section headings"
  , "[section]\nkey=var\n[section2]\nkey=var2\n[section]key\n"
  , \res -> and $ map ($res) [ valueIn stringVar secSection keyKey
                             , valueIn bTrue secSection keyKey
                             , not . valueIn stringVar2 secSection keyKey ]
  )
  ,

  ( "Two sections w/same name, multivalued"
  , "[section]\nkey=var\n[section]\nkey=var2\n"
  , valueIn stringVar2 secSection keyKey
  )
  ,

  ( "String: remove leading and trailing spaces, preserve inner spaces"
  , "[section]\nkey = val1 val2   \n"
  , valueIn (IString "val1 val2") secSection keyKey
  )
  ,

  ( "String: quoted strings (1)"
  , "[section]\nkey = \"   a quoted\n string \"\n"
  , valueIn (IString "   a quoted\n string ") secSection keyKey
  )
  ,

  ( "String: quoted strings (2)"
  , "[section]\nkey =\"half\" quoted \"string\"\n"
  , valueIn (IString "half quoted string") secSection keyKey
  )
  ,

  ( "String: escapes (1)"
  , "[section]\nkey=val\\\n continued\n"
  , valueIn (IString "val continued") secSection keyKey
  )
  ,

  ( "String: escapes (2)"
  , "[section]\nkey=val \\n \\t \\\\ \\\"\" \\\n asdf\"\n"
  , valueIn (IString "val \n \t \\ \"  asdf") secSection keyKey
  )
  ,

  ( "String: line continuations (1)"
  , "[section]\nkey = \\\n  var\n"
  , valueIn stringVar secSection keyKey
  )
  ,

  ( "String: line continuations (2)"
  , "[section]\nkey = var \\\n    \n"
  , valueIn stringVar secSection keyKey
  )
  ,

  ( "String: line continuations (3)"
  , "[section]\nkey = \\\n   var    \\\n    \n"
  , valueIn stringVar secSection keyKey
  )
  ,

  -- Zhiming Wang found this case
  ( "String: empty string, unquoted"
  , "[section]\nkey = \n"
  , valueIn stringEmpty secSection keyKey
  )
  ,

  -- Zhiming Wang found this case
  ( "String: empty string, quoted"
  , "[section]\nkey = \"\"\n"
  , valueIn stringEmpty secSection keyKey
  )
  ,

  -- Giovanni Campagna found this case
  ( "String: trailing whitespace at EoF"
  , "[section]\nkey = var "
  , valueIn stringVar secSection keyKey
  )
  ,

  ( "String: trailing whitespace at EoL"
  , "[section]\nkey = var \n "
  , valueIn stringVar secSection keyKey
  )
  ,

  ( "Comments (1)"
  , "#comment\n[section]\nkey\n"
  , valueIn bTrue secSection keyKey
  )
  ,

  ( "Comments (2)"
  , "[section]#comment\nkey\n"
  , valueIn bTrue secSection keyKey
  )
  ,

  ( "Comments (3)"
  , "[section]\n;comment\nkey\n"
  , valueIn bTrue secSection keyKey
  )
  ,

  ( "Comments (4)"
  , "[section]\nkey;comment\n"
  , valueIn bTrue secSection keyKey
  )
  ,

  ( "Comments (5)"
  , "#comment\n;comment\n\n\n;comment\n"
  , (==Right M.empty)
  )
  ,

  ( "Bool: key only"
  , "[section]\nkey\n"
  , valueIn bTrue secSection keyKey
  )
  ,

  ( "Bool: 'ON'"
  , "[section]\nkey=ON\n"
  , valueIn bTrue secSection keyKey
  )
  ,

  ( "Bool: 'TrUe'"
  , "[section]\nkey=TrUe\n"
  , valueIn bTrue secSection keyKey
  )
  ,

  ( "Bool: 'yeS'"
  , "[section]\nkey=yeS\n"
  , valueIn bTrue secSection keyKey
  )
  ,

  ( "Bool: 'off'"
  , "[section]\nkey=off\n"
  , valueIn bFalse secSection keyKey
  )
  ,

  ( "Bool: 'faLSe'"
  , "[section]\nkey=faLSe\n"
  , valueIn bFalse secSection keyKey
  )
  ,

  ( "Bool: 'No'"
  , "[section]\nkey=No\n"
  , valueIn bFalse secSection keyKey
  )
  ,

  ( "Bool: line continuations (1)"
  , "[section]\nkey =\\\n    true\n"
  , valueIn bTrue secSection keyKey
  )
  ,

  ( "Bool: line continuations (2)"
  , "[section]\nkey = false\\\n    \n"
  , valueIn bFalse secSection keyKey
  )
  ,

  ( "Bool: line continuations (3)"
  , "[section]\nkey = \\\n    true    \\\n    \n"
  , valueIn bTrue secSection keyKey
  )
  ,

  -- Zhiming Wang (correctly) points out that this test is at odds with
  -- README.md, which says that only *values* can have line continuations.
  --( "Bool: line continuations (4)"
  --, "[section]\nkey   \\\n  \n"
  --, valueIn bTrue secSection keyKey
  --)
  --,

  -- Giovanni Campagna found this case
  ( "Bool: trailing whitespace at EoF"
  , "[section]\nkey = False "
  , valueIn bFalse secSection keyKey
  )
  ,

  ( "Bool: trailing whitespace at EoL"
  , "[section]\nkey = False \n "
  , valueIn bFalse secSection keyKey
  )
  ,

  ( "Integer: positive"
  , "[section]\nkey=1234\n"
  , valueIn (IInt 1234) secSection keyKey
  )
  ,

  ( "Integer: negative"
  , "[section]\nkey=-12345\n"
  , valueIn (IInt (-12345)) secSection keyKey
  )
  ,

  ( "Integer: suffix (k)"
  , "[section]\nkey=1k\n"
  , valueIn (IInt 1024) secSection keyKey
  )
  ,

  ( "Integer: suffix (M)"
  , "[section]\nkey=1M\n"
  , valueIn (IInt 1048576) secSection keyKey
  )
  ,

  ( "Integer: suffix (G)"
  , "[section]\nkey=1G\n"
  , valueIn (IInt 1073741824) secSection keyKey
  )
  ,

  ( "Integer: suffix (T)"
  , "[section]\nkey=1T\n"
  , valueIn (IInt 1099511627776) secSection keyKey
  )
  ,

  ( "Integer: suffix (P)"
  , "[section]\nkey=1P\n"
  , valueIn (IInt 1125899906842624) secSection keyKey
  )
  ,

  ( "Integer: suffix (E)"
  , "[section]\nkey=1E\n"
  , valueIn (IInt 1152921504606846976) secSection keyKey
  )
  ,

  ( "Integer: floats should be interpreted as strings"
  , "[section]\nkey=123.45\n"
  , valueIn (IString "123.45") secSection keyKey
  )
  ,

  ( "Integer: line continuations (1)"
  , "[section]\nkey=\\\n      123\n"
  , valueIn (IInt 123) secSection keyKey
  )
  ,

  ( "Integer: line continuations (2)"
  , "[section]\nkey= -432\\\n    \n"
  , valueIn (IInt (-432)) secSection keyKey
  )
  ,

  ( "Integer: line continuations (3)"
  , "[section]\nkey = \\\n    -1024    \\\n    \n"
  , valueIn (IInt (-1024)) secSection keyKey
  )
  ,

  -- Giovanni Campagna found this case
  ( "Integer: trailing whitespace at EoF"
  , "[section]\nkey = -1P "
  , valueIn (IInt (-1125899906842624)) secSection keyKey
  )
  ,

  ( "Integer: trailing whitespace at EoL"
  , "[section]\nkey = -1P \n "
  , valueIn (IInt (-1125899906842624)) secSection keyKey
  )
  ,

  ( "Subsection case sensitivity (1)"
  , "[section \"SUBSECTION\"]\nkey\n"
  , not . valueIn bTrue subSectionLC keyKey
  )
  ,

  ( "Subsection case sensitivity (2)"
  , "[section \"SUBSECTION\"]\nkey\n"
  , valueIn bTrue subSectionUC keyKey
  )
  ,

  ( "Subsection escapes (1)"
  , "[section \"SUBSECTION \\\"1\\\"\"]\nkey\n"
  , valueIn bTrue subSectionEsc1 keyKey
  )
  ,

  ( "Subsection escapes (2)"
  , "[section \"SUBSECTION \\\\2\\\\\"]\nkey\n"
  , valueIn bTrue subSectionEsc2 keyKey
  )
  ,

  ( "Subsection - no newlines allowed (1)"
  , "[section \"SUBSECTION \n\"]"
  , isLeft
  )
  ,

  ( "Subsection - no newlines allowed (2)"
  , "[section \"SUBSECTION \\n\"]"
  , isLeft
  )
  ,

  ( "Subsection - no newlines allowed (3)"
  , "[section \"SUBSECTION \\\n\"]"
  , isLeft
  )
  ,

  ( "File without newlines (string)"
  , "[section] key=var"
  , valueIn stringVar secSection keyKey
  )
  ,

  ( "File without newlines (bool)"
  , "[section] key=False"
  , valueIn bFalse secSection keyKey
  )
  ,

  ( "File without newlines (integer)"
  , "[section] key=1M"
  , valueIn (IInt 1048576) secSection keyKey
  )
  ]




-- **** Generating arbitrary INI files with QuickCheck ****

instance Monoid (Gen Builder) where
    mempty = return mempty
    mappend a b = do aa <- a
                     bb <- b
                     return $ aa <> bb

-- |Generate a uniformly distributed alphabetic character.
arbAlpha :: Gen Char
arbAlpha = frequency [ (26, choose('A', 'Z'))
                     , (26, choose('a', 'z')) ]

-- |Generate a uniformly distributed numeric character.
arbNum :: Gen Char
arbNum = choose ('0', '9')

-- |Generate a uniformly distributed alphanumeric character.
arbAlNum :: Gen Char
arbAlNum = frequency [ (52, arbAlpha)
                     , (10, arbNum) ]

-- |Generate a uniformly distributed character from the set allowed in keys.
arbKeyChar :: Gen Char
arbKeyChar = frequency [ (62, arbAlNum)
                       , (1, return '-') ]

-- |Generate a uniformly distributed character from the set allowed in section names.
arbSectChar :: Gen Char
arbSectChar = frequency [ (63, arbKeyChar)
                        , (1, return '.') ]

-- |Generate a character from the set allowed in subsection names.
-- Control characters are 10x less likely than printable characters.
--
-- Note that this function will return \ and ", which need to be escaped!
-- In addition, it will return characters >= \128, which we use as
-- placeholders for escaped \\t, \\b.
arbSubsectChar :: Gen Char
arbSubsectChar = frequency [ (95, choose (' ', '~'))
                           , (1, choose ('\128', '\129'))
                           , (3, frequency [ (10, choose ('\0', '\9'))
                                           , (2 , choose ('\v', '\f'))
                                           , (18, choose ('\14', '\31'))
                                           , (1, return '\127')
                                           ])
                           ]

-- |Generate a character from the set allowed in quoted values.
-- Control characters are 10x less likely than printable characters.
--
-- Note that this function will return \ and ", which need to be escaped!
-- In addition, it will return characters >= \128, which we use as
-- placeholders for escaped \\t, \\b, \\n, \\\n.
arbQuotedChar :: Gen Char
arbQuotedChar = frequency [ (495, arbSubsectChar)
                          , (5, choose ('\130', '\131'))
                          , (1, return '\n')
                          , (1, return '\r')
                          ]

-- |Generate a character from the set allowed in unquoted values.
-- Control characters are 10x less likely than printable characters.
--
-- Note that this function will return \ and ", which need to be escaped!
-- In addition, it will return characters >= \128, which we use as
-- placeholders for escaped \\t, \\b, \\n, \\\n.
arbUnquotedChar :: Gen Char
arbUnquotedChar = frequency [ (2, choose ('!', '"'))
                            , (23, choose ('$', ':'))
                            , (67, choose ('<', '~'))
                            , (2, choose ('\128', '\131'))
                            , (3, frequency [ (9, choose ('\0', '\8'))
                                            , (2, choose ('\11', '\12'))
                                            , (18, choose ('\14', '\31'))
                                            ])
                            ]

-- |Generate an arbitrary whitespace character.
arbWSChar :: Gen Char
arbWSChar = frequency [ (10, return ' ') ]
                      --, (1, elements ['\t', '\v', '\f']) ]

-- |Given a character generator, make a builder generator that escapes characters.
escArbChar :: Gen Char -> Gen Builder
escArbChar gen = do c <- gen
                    return $ case c of
                        '\\' -> string7 "\\\\"
                        '\"' -> string7 "\\\""
                        '\128' -> string7 "\\t"
                        '\129' -> string7 "\\b"
                        '\130' -> string7 "\\n"
                        '\131' -> string7 "\\\n"
                        _ -> char7 c

-- |Given a character generator, return a Builder generating a string of escaped chars.
arbCBuildGen :: Gen Char -> Gen Builder
arbCBuildGen = arbBuildGen . escArbChar
  where arbBuildGen b = do chars <- listOf1 b
                           return $ foldl (<>) mempty chars

-- |Given a builder, put double quotes around it.
arbQBuildGen :: Gen Builder -> Gen Builder
arbQBuildGen b = do str <- b
                    return $ char7 '"' <> str <> char7 '"'

-- |Generates an arbitrary sequence of whitespace.
arbWhite :: Gen Builder
arbWhite = string7 <$> listOf1 arbWSChar

-- |Generates an arbitrary whitespace character, including escaped newlines
arbWhiteQ :: Gen Builder
arbWhiteQ = arbCBuildGen arbW
  where arbW = frequency [ (50, arbWSChar)
                         , (1, return '\131') ]

-- |Generates an arbitrary comment string builder.
arbComment :: Gen Builder
arbComment = do cStr <- arbCBuildGen arbSubsectChar
                comChar <- elements [';', '#']
                return $ char7 comChar <> cStr

-- |Generate an arbitrary section string.
arbSect :: Gen Builder
arbSect = string7 <$> listOf1 arbSectChar

-- |Build an arbitrary subsection string.
arbSubSect :: Gen Builder
arbSubSect = arbQBuildGen $ arbCBuildGen arbSubsectChar

-- |Build an arbitrary section name
arbSectName :: Gen Builder
arbSectName = do 
                 -- the commented lines below allow whitespace between [ and
                 -- start of section name; or between end of section and ].
                 -- There's no particular reason to think this is invalid,
                 -- but it wasn't clear either way from the git-config manual.
                 --
                 --preWS <- arbMWhite
                 sect <- oneof [arbSect, arbSect <> arbWhite <> arbSubSect]
                 --postWS <- arbMWhite
                 --return $ char7 '[' <> preWS <> sect <> postWS <> char7 ']'
                 return $ char7 '[' <> sect <> char7 ']'
  where arbMWhite = oneof [arbWhite, mempty]

-- |Build an arbitrary quoted string.
arbQuoted = arbQBuildGen $ arbCBuildGen arbQuotedChar

-- |Build an arbitrary unquoted string.
arbUnquoted = arbCBuildGen arbUnquotedChar

-- |Build an arbitrary string value.
arbStringVal :: Gen Builder
arbStringVal = do first <- arbQorUnQ
                  rest <- listOf arbPair
                  return $ first <> foldl (<>) mempty rest
  where arbQorUnQ = oneof [arbQuoted, arbUnquoted]
        arbPair = arbWhiteQ <> arbQorUnQ

-- |Build an arbitrary integer value.
arbIntVal :: Gen Builder
arbIntVal = do sign <- elements [mempty, char7 '+', char7 '-']
               num <- string7 <$> listOf1 arbNum
               suffix <- char7 <$> elements ['k', 'M', 'G', 'T', 'P', 'E']
               return $ sign <> num <> suffix

-- |Build an arbitrary boolean value.
arbBoolVal :: Gen Builder
arbBoolVal = string7 <$> ( zipWith flipCase <$> boolword <*> flips )
  where flipCase c b = if b then toUpper c else c
        flips = infiniteListOf $ elements [True, False]
        boolword = oneof [ elements ["true", "yes", "on"]
                         , elements ["false", "no", "off"] ]

-- |Build an arbitrary key name.
arbKeyName :: Gen Builder
arbKeyName = do first <- arbAlpha
                rest <- listOf arbKeyChar
                return $ char7 first <> string7 rest

-- |Build an arbitrary key-value pair.
arbKeyValue :: Gen Builder
arbKeyValue = do ws1 <- arbMWhite
                 keyName <- arbKeyName
                 ws2 <- arbMWhite
                 ws3 <- arbMWhiteQ
                 val <- oneof [arbStringVal, arbIntVal, arbBoolVal]
                 ws4 <- arbMWhiteQ
                 trailing <- oneof [ arbComment, mempty ]
                 -- either a key = value, or a key
                 elements [ ws1 <> keyName <> ws2 <> char7 '=' <> ws3 <> val <> ws4 <> trailing <> char7 '\n'
                          -- the commented line below allows key-only lines to have line continuations,
                          -- which is at odds with the specification given in the assignment.
                          --, ws1 <> keyName <> ws4 <> trailing <> char7 '\n'
                          , ws1 <> keyName <> ws2 <> trailing <> char7 '\n'
                          ]
  where arbMWhite = oneof [arbWhite, mempty]
        arbMWhiteQ = oneof [arbWhite, arbWhiteQ, mempty]

-- |Build an arbitrary section.
arbSection :: Gen Builder
arbSection = do ws1 <- arbMWhite
                sectName <- arbSectName
                sectSep <- elements [char7 '\n', mempty]
                kvPairs <- zipWith (<>) <$> listOf arbKeyValue <*> wss
                let keyValues = foldl (<>) mempty kvPairs
                return $ ws1 <> sectName <> sectSep <> keyValues
  where wss = infiniteListOf (arbWhite <> elements [char7 '\n', mempty])
        arbMWhite = oneof [arbWhite, mempty]

-- |Build a builder that dumps out an arbitrary INI file.
arbIni :: Gen Builder
arbIni = listOf arbSection >>= return . foldl (<>) mempty

-- |Execute a builder that dumps out an arbitrary INI file.
arbIniFile :: Gen B.ByteString
arbIniFile = arbIni >>= return . BL.toStrict . toLazyByteString
