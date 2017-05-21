module Main
    ( main
    ) where

import ParseIni
import PrettyPrintIni

import Control.Monad (liftM)
import Data.ByteString.Char8 as B
import Data.Map.Strict as M
import Test.Hspec
import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Property (failed, liftBool, Result)

-- |Run all tests.
main :: IO ()
main = hspec $ describe "Testing parser" $ do
    -- Check that an empty file returns an empty map.
    it "Empty file" $ parseIniFile B.empty `shouldBe` Right M.empty

    -- Check that the pretty printer is idempotent through parsing,
    -- and that the parse returns the correct result.
    it "Pretty-printer idempotence" $ testIdemp B.empty `shouldBe` Right M.empty
    -- *NOTE* that the above test only checks for the empty case.
    -- Once you have implemented your parser and pretty-printer,
    -- you should check for idempotence on more interesting cases.

    -- Check that the parser correctly saves every value for a multivalued variable.
    it "Correct count for multivalued variables" $ property pMultivalCount
    -- The above test will fail until you've implemented your parser.

    -- You will need more tests, both hspec and QuickCheck based.
    -- Hint: think about how you can generate random INIFiles using your
    -- pretty printer and QuickCheck, and/or by writing a grammar for
    -- generating INI files directly.

-- |Test the pretty-printer by checking for idempotence, then returning parsed result.
testIdemp :: B.ByteString -> Either String INIFile
testIdemp s | run1 == run2 = parseRes
            | otherwise = errorRes
  where run1 = liftM prettyPrint $ parseIniFile s
        run2 = liftM prettyPrint $ parseIniFile =<< run1
        parseRes = parseIniFile =<< run2
        errorRes = Left "Pretty printer failed idempotence check."

-- |Look up a value in an INIFile.
lookupVal :: INISectName -> INIKey -> INIFile -> [INIVal]
lookupVal sect key file = M.findWithDefault [] key $ M.findWithDefault M.empty sect file

-- **** QuickCheck tests ****
type MultivalCount = NonNegative Int

-- |Test that, given an INI file with some number of repeated @key=val@ declarations,
-- the resulting parse has that number of values in the @INIFile@.
pMultivalCount :: MultivalCount -> Result
pMultivalCount c | (Left _) <- eParsedOut = failed
                 | otherwise = lengthMatches
  where -- construct input stream that declares a multivalued variable
        (NonNegative cInt) = c
        keyDecls = Prelude.take cInt $ repeat "key=val\n"
        inFile = B.pack $ "[section]\n" ++ Prelude.concat keyDecls

        -- parse the generated input stream
        eParsedOut = parseIniFile inFile
        (Right parsedOut) = eParsedOut

        -- extract the key from the parsed output
        sect = toSectName "section" Nothing
        key = toKey "key"
        values = lookupVal sect key parsedOut

        -- length of values should match value of c
        lengthMatches = liftBool $ cInt == Prelude.length values
