{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
module Parser where

-- import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char

infixr 5 :<

pattern (:<) :: Char -> C.ByteString -> C.ByteString
pattern b :< bs <- (C.uncons -> Just (b, bs))
pattern Empty :: C.ByteString
pattern Empty   <- (C.uncons -> Nothing)

type Input = C.ByteString
type Str = C.ByteString

data ParseError = Failed
  deriving (Eq, Show)

data ParseResult a = ErrorResult ParseError
                   | Result Input a

newtype Parser a = P {
                     parse :: Input -> ParseResult a
                     }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f pa = pa >>= (valueParser . f)

instance Applicative Parser where
  pure :: a -> Parser a
  pure = valueParser
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) pf pa = pf >>= (\f -> pa >>= (pure . f))

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) pa f = P (\inp -> case parse pa inp of
                          ErrorResult e -> ErrorResult e
                          Result inp' a -> parse (f a) inp')


-- | Consume no input, succeed with given value
valueParser :: a -> Parser a
valueParser out = P (`Result` out)

-- | Consume no input, then fail
failed :: Parser a
failed = P (\_ -> ErrorResult Failed)

-- | Consume a character, fail if input is empty
charParser :: Parser Char
charParser = P (\inp -> case inp of
                         Empty -> ErrorResult Failed
                         (c :< cs) -> Result cs c)

-- | Consume and discard input from first parser, continue with second parser
(>>>) :: Parser a -> Parser b -> Parser b
pa >>> pb = pa >>= const pb

-- | Try the first parser, and if it fails, try the second
(|||) :: Parser a -> Parser a -> Parser a
(|||) p1 p2 = P (\inp -> case parse p1 inp of
                           ErrorResult _ -> parse p2 inp
                           Result inp' a -> Result inp' a)
infixl 3 |||

-- | Parse zero or more values
list :: Parser Char -> Parser Str
list pa = list1 pa ||| valueParser C.empty

-- | Parse one or more values
list1 :: Parser Char -> Parser Str
list1 pa = do
  a <- pa
  as <- list pa
  return $ C.cons a as

-- | Parse a char that satisfy the given predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = charParser >>= \a ->
            if p a
              then valueParser a
              else failed

-- | Character equals
is :: Char -> Parser Char
is c = satisfy (==c)

-- | Character equals, case insensitive
isNoC :: Char -> Parser Char
isNoC c = satisfy (\c' -> c' == toLower c || c' == toUpper c)

-- | String equals
isStr :: Str -> Parser Str
isStr = C.foldr f (valueParser C.empty)
  where
    f c pcs = do
      c' <- is c
      cs <- pcs
      return $ C.cons c' cs

-- | String equals, case insensitive
isStrNoC :: Str -> Parser Str
isStrNoC = C.foldr f (valueParser C.empty)
  where
    f c pcs = do
      c' <- isNoC c
      cs <- pcs
      return $ C.cons c' cs


-- | Parse a character which appears in the given string
oneOf :: Str -> Parser Char
oneOf str = satisfy (`C.elem` str)

-- | Parse a character which do not appear in the given string
noneOf :: Str -> Parser Char
noneOf str = satisfy (`C.notElem` str)

-- | Consume a character (left), then run a parser, then consume a character (right)
betweenChars :: Char -> Char -> Parser a -> Parser a
betweenChars l r pa = do
  _ <- is l
  res <- pa
  _ <- is r
  return res

alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum
