{-# LANGUAGE DeriveDataTypeable #-}

-- | Support for simple, non-incremental parsers on ByteStrings that
-- tell you on what line a parse error occured.
module Parser (Parser(..), ParseError(..), parse, runParser, tryParse
              , parseChar, char, satisfy, skip
              , parseWhile, parseWhile1, skipWhile, skipWhile1
              , isHSpace, isBetween
              , parseEOF, peekParse, maybeParse
              , parseBool, parseIntegral
              ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Char
import Data.Typeable

-- | Simple parser monad for 'S.ByteString's fully contained in
-- memory.
newtype Parser a = Parser {
    -- | The argument is the input to parse.  On failure, the return
    -- value is 'Left' with input at the point of error.  On success,
    -- the return value is 'Right' with result and remaining unparsed
    -- data.
    unParser :: S.ByteString -> Either S.ByteString (a, S.ByteString)
  }
instance Monad Parser where
  return a = Parser $ \s -> Right (a, s)
  m >>= k = Parser $ \s0 -> case unParser m s0 of
    Right (b, s) -> unParser (k b) s
    Left s -> Left s
  fail _ = mzero
instance Functor Parser where fmap = liftM
instance Applicative Parser where
  pure = return
  (<*>) = ap
instance Alternative Parser where
  empty = Parser Left
  a <|> b = Parser $ \s -> case unParser a s of
    Left _ -> unParser b s
    right  -> right
instance MonadPlus Parser

-- | Exception thrown on parse errors.  Contains the pathname of the
-- file that caused the parse error, and the line number of at which
-- the error occurred.
data ParseError = ParseError FilePath Int
                deriving (Typeable)
instance Show ParseError where
  showsPrec _ (ParseError file line) rest =
    file ++ ':' : show line ++ ": parse error" ++ rest
instance Exception ParseError

-- | Run a parser an a particular input string.  If the parser
-- fails or does not fully consume the input, return a 'Left'
-- 'ParseError'.  Otherwise, return 'Right' the result of the parse.
runParser :: Parser a  -- ^ Parser to run
          -> FilePath  -- ^ Source of data (for annotating exceptions)
          -> S.ByteString       -- ^ Input to parse
          -> Either ParseError a
runParser pa file bs =
  case unParser (pa <* parseEOF) bs of
   Right (a, _) -> Right a
   Left rest -> let errpos = S.length bs - S.length rest
                    prefix = S.take errpos bs
                    lineno = 1 + S8.count '\n' prefix
                in Left $ ParseError file lineno

-- | Run a parser on a particular input string.  If the parser
-- fails or does not fully consume the input, throws a 'ParseError'.
parse :: Parser a          -- ^ Parser to run
      -> FilePath          -- ^ Source of data (only used in exceptions)
      -> S.ByteString      -- ^ Input to parse
      -> a
parse pa file bs = either throw id $ runParser pa file bs

-- | Runs a parser on a ByteString like 'parse', but returns 'Nothing'
-- on error instead of throwing an exception.
tryParse :: Parser a -> S.ByteString -> Maybe a
tryParse p s = either (const Nothing) Just $ runParser p "" s

-- | Run a parser and return its value without consuming any input.
peekParse :: Parser a -> Parser a
peekParse p = Parser $ \s ->
  case unParser p s of
   Right (a, _) -> Right (a, s)
   left -> left

-- | Run a parser but return 'Nothing' if it fails, instead of 
-- making the invoking 'Parser' fail.
maybeParse :: Parser a -> Parser (Maybe a)
maybeParse p = Just <$> p <|> return Nothing

{-
saveParse :: Parser a -> Parser (a, S.ByteString)
saveParse p = Parser $ \s0 ->
  case unParser p s0 of
   Right (a, s) -> Right ((a, S.take (S.length s0 - S.length s) s0), s)
   Left s -> Left s

inputOfParse :: Parser a -> Parser S.ByteString
inputOfParse p = snd <$> saveParse p
-}

-- | True if character is space or tab.
isHSpace :: Char -> Bool
isHSpace c = c == ' ' || c == '\t'

-- | Test if a character is within a range.  E.g., @isBetween '0' '9'@
-- tests if characters are numeric digits.
isBetween :: Char -- ^ Lowest valid character
          -> Char -- ^ Highest valid caracter
          -> Char -- ^ Character to test
          -> Bool -- ^ True if lowest <= char-to-test <= highest
isBetween lo0 hi0 w = w >= lo && w <= hi
  where lo = toEnum $ ord lo0
        hi = toEnum $ ord hi0

-- | Returns the next character if it matches a predicate, fails if
-- it doesn't match or if the end of file has been reached.
satisfy :: (Char -> Bool) -> Parser Char
satisfy predi = Parser $ \s -> case S8.uncons s of
  Just cr@(c,_) | predi c -> Right cr
  _                       -> Left s

-- | Returns the next character if it matches a predicate, fails if
-- it doesn't match or if the end of file has been reached.
skip :: (Char -> Bool) -> Parser ()
skip predi = Parser $ \s -> case S8.uncons s of
  Just (c,r) | predi c -> Right ((), r)
  _                    -> Left s

-- | Returns 0 or more characters matching a predicate.  Never fails.
parseWhile :: (Char -> Bool) -> Parser S.ByteString
parseWhile predi = Parser $ Right . S8.span predi

-- | Returns 1 or more characters matching a predicate.
parseWhile1 :: (Char -> Bool) -> Parser S.ByteString
parseWhile1 predi = do
  r <- parseWhile predi
  if S.null r then mzero else return r

-- | Discards 0 or more input characters matching a predicate.  Never
-- fails.
skipWhile :: (Char -> Bool) -> Parser ()
skipWhile predi = Parser $ \s -> Right ((), S8.dropWhile predi s)

-- | Discards 1 or more input characters matching a predicate, or
-- fails if the next character does not match the predicate.
skipWhile1 :: (Char -> Bool) -> Parser ()
skipWhile1 predi = do
  skipped <- parseWhile predi
  when (S.null skipped) mzero

-- | Match an exact character, or fail
char :: Char -> Parser Char
char target = Parser $ \s -> case S8.uncons s of
  Just cr@(c,_) | c == target -> Right cr
  _                           -> Left s

-- | Returns the next character, or fails if no input is left.
parseChar :: Parser Char
parseChar = Parser $ \s -> maybe (Left s) Right $ S8.uncons s

-- | Fails if there is any input left to parse.
parseEOF :: Parser ()
parseEOF = Parser $ \s -> if S.null s then Right ((), s) else Left s

-- | Parse a boolean.  Accepts @\"false\"@, @\"no\"@, @\"off\"@, and
-- @\"0\"@ as 'False', and @\"true\"@, @\"yes\"@, @\"on\"@, and
-- @\"1\"@ as 'True'.  Matches in a case-insentitive way.
parseBool :: Parser Bool
parseBool = do
  v <- S8.map toLower <$> parseWhile isAlphaNum
  case () of
   _ | v `elem` trues  -> return True
     | v `elem` falses -> return False
     | otherwise       -> mzero
  where falses = map S8.pack ["false", "no", "off", "0"]
        trues = map S8.pack ["true", "yes", "on", "1"]

-- | Parse an Integral type, without doing anything particularly
-- intelligent should there be an arithmetic overflow.
parseIntegral :: (Integral a) => Parser a
parseIntegral = nextDigit 0 >>= go
  where go n = nextDigit (n * 10) <|> return n
        nextDigit n = do
          d <- digit
          go (n + d)
        digit = do
          c <- parseChar
          unless (isBetween '0' '9' c) mzero
          return $ fromIntegral $ ord c - ord '0'

