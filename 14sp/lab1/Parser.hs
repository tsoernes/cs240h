{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}

module Parser where

type Input = String

data ParseError = Failed
  deriving (Eq, Show)

data ParseResult a = ErrorResult ParseError
                   | Result Input a
                   deriving (Eq, Functor)

instance Show a => Show (ParseResult a) where
  show (ErrorResult e) = show e
  show (Result i a) = "Result >" ++ i ++ "< " ++ show a

showRes :: Show a => ParseResult a -> String
showRes (ErrorResult e) = show e
showRes (Result _ a) = show a

newtype Parser a = P {
                     parse :: Input -> ParseResult a
                     }

-- | Consume no input, succeed with given value
valueParser :: a -> Parser a
valueParser out = P (`Result` out)

-- | Consume no input, succeed with a list with the given value
valueParserS :: a -> Parser [a]
valueParserS out = P (`Result` [out])

-- | Consume no input and succeed with a parser of a list
valueInpParser :: Parser a -> Parser (Parser [a])
valueInpParser p = valueParser $ (:[]) <$> p

-- | Consume no input, then fail
failed :: Parser a
failed = P (\_ -> ErrorResult Failed)

-- | Consume a character, fail if input is empty
charParser :: Parser Char
charParser = P (\inp -> case inp of
                         [] -> ErrorResult Failed
                         (c:cs) -> Result cs c)

-- | Consume and discard input from first parser, continue with second parser
(>>>) :: Parser a -> Parser b -> Parser b
pa >>> pb = pa >>= const pb

-- | Try the first parser, and if it fails, try the second
(|||) :: Parser a -> Parser a -> Parser a
(|||) p1 p2 = P (\inp -> case parse p1 inp of
                           ErrorResult _ -> parse p2 inp
                           Result i a -> Result i a)
infixl 3 |||

-- | Parse one or more values
list :: Parser a -> Parser [a]
list pa = list1 pa ||| valueParser []

-- | Parse one or more values
list1 :: Parser a -> Parser [a]
list1 pa = do
  a <- pa
  as <- list pa
  return $ a:as

-- | Parse zero or more strings, then concatenate the result
concatLi :: Parser String -> Parser String
concatLi pa = concatLi1 pa ||| valueParser []

-- | Parse one or more strings, then concatenate the result
concatLi1 :: Parser String -> Parser String
concatLi1 pa = list1 pa >>= (valueParser . concat)

-- | Parse one element, then a list of elements, then cons the result
consParsers :: Parser a -> Parser [a] -> Parser [a]
consParsers pa pas = do
  a <- pa
  as <- pas
  return $ a:as

-- | Run all parsers in the list in sequence
sequenceParser :: [Parser a] -> Parser [a]
sequenceParser = foldr f (valueParser [])
  where
    f pa pas = pa >>= \a ->
               pas >>= \as ->
               valueParser (a:as)

sequenceAnyParser :: [Parser (Parser String)] -> Parser (Parser String)
sequenceAnyParser = foldr f (valueParser (valueParser []))
  where
    f pa pas = pa >>= \a ->
               pas >>= \as ->
                 valueParser (a ||| as)

-- | Succeed on first parser in the list
anyParser :: [Parser a] -> Parser a
anyParser = foldr (|||) failed

-- | Run all parsers in the list in sequence and concatenate the result
sequenceParserConcat :: [Parser [a]] -> Parser [a]
sequenceParserConcat ps = concat <$> sequenceParser ps

-- | Parse a char that satisfy the given predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = charParser >>= \a ->
            if p a
              then valueParser a
              else failed

-- | Character equals
is :: Char -> Parser Char
is c = satisfy (==c)

-- | Parse a character which appears in the given string
oneOf :: String -> Parser Char
oneOf str = satisfy (`elem` str)

-- | Parse a character which do not appear in the given string
noneOf :: String -> Parser Char
noneOf str = satisfy (`notElem` str)

-- | Consume a character (left), then run a parser, then consume a character (right)
betweenChars :: Char -> Char -> Parser a -> Parser a
-- betweenChars l r pa = is l *> pa <* is r
betweenChars l r pa = do
  _ <- is l
  res <- pa
  _ <- is r
  return res

eof :: Parser [a]
eof = P f
 where
   f [] = Result [] []
   f _ = ErrorResult Failed

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f pa = pa >>= (valueParser . f)

instance Applicative Parser where
  pure :: a -> Parser a
  pure = valueParser
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) pf pa = pf >>= (\f -> pa >>= (valueParser . f))

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) pa f = flBindP f pa

-- | Parser bind with flipped arguments
flBindP :: (a -> Parser b) -> Parser a -> Parser b
flBindP f pa = P (\inp -> case parse pa inp of
                          ErrorResult e -> ErrorResult e
                          Result i a -> parse (f a) i)

