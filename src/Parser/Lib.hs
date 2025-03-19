{-
-- EPITECH PROJECT, 2024
-- Lib
-- File description:
-- Glados
-}

module Parser.Lib
    ( Parser(..)
    , ParserError(..)
    , parseChar
    , parseAnyChar
    , parseString
    , parseOr
    , parseAnd
    , parseAndWith
    , parseAndWith3
    , parseMany
    , parseSome
    , parseUInt
    , parseInt
    , parseTuple
    , parseUselessChar
    , parseEmpty
    , parseBool
    , parseManyWithError
    , parseFloat
    ) where

import Control.Applicative

-- on the Left Just side of the Either -> the error token and the rest of the string
-- on the right side of the Either -> the result and the rest of the string

data ParserError = 
    Missing (String, String, String)
    deriving (Show, Eq)

data Parser a = Parser {
    runParser :: String -> Either ParserError (a, String)
}

instance Functor Parser where
    fmap f (Parser p) = Parser $ \str -> case p str of
        Left err     -> Left err
        Right (a, b) -> Right (f a, b)

instance Applicative Parser where
    pure a = Parser $ \str -> Right (a, str)
    (Parser p1) <*> (Parser p2) = Parser $ \str -> case p1 str of
        Left err     -> Left err
        Right (a, b) -> case p2 b of
            Left err     -> Left err
            Right (c, d) -> Right (a c, d)

instance Monad Parser where
    return = pure
    (Parser p1) >>= f = Parser $ \str -> case p1 str of
        Left err     -> Left err
        Right (a, b) -> runParser (f a) b

instance Alternative Parser where
    empty = Parser $ \_ -> Left $ Missing("undi", "_", "empty alternative")
    (Parser p1) <|> (Parser p2) = Parser $ \str -> case p1 str of
        Left _       -> p2 str
        Right (a, b) -> Right (a, b)

-- Function to parse a character
parseChar :: Char -> Parser Char
parseChar a = Parser $ \str -> case str of
    [] -> Left $ Missing ([a], str, "parseChar")
    (h:t)
        | a == h    -> Right (a, t)
        | otherwise -> Left $ Missing ([a], str, "parseChar")

-- Function to parse any character
parseAnyChar :: String -> Parser Char
parseAnyChar str = Parser $ \s -> case s of
    [] -> Left $ Missing ("_", str, "parseAnyChar")
    (h:t)
        | h `elem` str  -> Right (h, t)
        | otherwise     -> Left $ Missing (h:t, str, "parseAnyChar")

-- Function to parse a string
parseString :: String -> Parser String
parseString str = Parser $ \s -> case runParser (
    parseSome (parseAnyChar str)
    ) s of
    Left (Missing(a, b, c)) -> Left (Missing(a, b, c ++ " -> parseString"))
    Right (a, b)            -> case a == str of
        True  -> Right (a, b)
        False -> Left $ Missing (a, b, "parseString")

-- Function to parse two elements  
parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 = p1 <|> p2

-- Function to parse two elements
parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 = Parser $ \str -> case runParser p1 str of
    Left (Missing(a, b, c)) -> Left (Missing(a, b, c ++ " -> parseAnd"))
    Right (a, b)            -> case runParser p2 b of 
        Left (Missing(ar, br, c))   -> Left (Missing(ar, br, c ++ " -> parseAnd"))
        Right (c, d)                -> Right ((a, c), d)
        
-- Function to parse two elements and apply a function to them
parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 = f <$> p1 <*> p2

-- Function to parse three elements and apply a function to them
parseAndWith3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
parseAndWith3 f p1 p2 p3 = f <$> p1 <*> p2 <*> p3

-- Function to parse many elements
parseMany :: Parser a -> Parser [a]
parseMany p = Parser $ \str -> case runParser p str of
    Left _       -> Right ([], str)
    Right (a, b) -> case runParser (parseMany p) b of
        Left _       -> Right ([a], b)
        Right (c, d) -> Right (a:c, d)

-- Function to parse many elements with error
parseManyWithError :: Parser a -> Parser [a]
parseManyWithError p = Parser $ \str -> case runParser p str of
    Left err     -> Left err
    Right (a, b) -> case runParser (parseManyWithError p) b of
        Left _       -> Right ([a], b)
        Right (c, d) -> Right (a:c, d)

-- Function to parse some elements
parseSome :: Parser a -> Parser [a]
parseSome p = (:) <$> p <*> parseMany p

-- Function to parse an unsigned integer
parseUInt :: Parser Int
parseUInt = Parser $ \str -> case runParser (
    parseSome (parseAnyChar ['0'..'9'])
    ) str of
    Left (Missing(a, b, c)) -> Left (Missing(a, b, c ++ " -> parseUInt"))
    Right (a, b)            -> Right (read a, b)

-- Function to parse an integer
parseInt :: Parser Int
parseInt = parseUInt <|> (negate <$> (parseChar '-' *> parseUInt))

-- Function to parse a tuple
parseTuple :: Parser a -> Parser (a, a)
parseTuple p = Parser $ \str -> case runParser (parseChar '(' *> parseUselessChar *> p) str of
    Left (Missing(a, b, c)) -> Left (Missing(a, b, c ++ " -> parseTuple"))
    Right (a, rest1)        -> case runParser (parseUselessChar *> parseChar ',' *> parseUselessChar *> p) rest1 of
        Left (Missing(ar, b, c)) -> Left (Missing(ar, b, c ++ " -> parseTuple"))
        Right (b, rest2)         -> case runParser (parseUselessChar *> parseChar ')') rest2 of
            Left (Missing(ar, br, c)) -> Left (Missing(ar, br, c ++ " -> parseTuple"))
            Right (_, rest3)          -> Right ((a, b), rest3)

-- Function to parse useless characters
parseUselessChar :: Parser String
parseUselessChar = Parser $ \str -> case runParser (parseMany (parseAnyChar " \n\t")) str of
    Left (Missing(a, _, _)) -> Right (a, str)
    Right (a, b)            -> Right (a, b)

-- Function to parse an empty string
parseEmpty :: Parser String -> Parser String
parseEmpty p = Parser $ \str -> case runParser p str of
    Left err     -> Left err
    Right (a, b) -> Right (a, b)

-- Function to parse a boolean
parseBool :: Parser Bool
parseBool = Parser $ \str -> case runParser (
    parseString "#t" <|> parseString "#f"
    ) str of
    Left (Missing(a, b, c)) -> Left (Missing(a, b, c ++ " -> parseBool"))
    Right (a, b)            -> case a of
        "#t" -> Right (True, b)
        "#f" -> Right (False, b)
        res  -> Left $ Missing (res, str, "parseBool")

-- Function to parse a float
parseFloat :: Parser Float
parseFloat = Parser $ \str -> case runParser (
    parseMany (parseAnyChar ['0'..'9'])
    ) str of
    Left (Missing(a, b, c)) -> Left (Missing(a, b, c ++ " -> parseFloat"))
    Right (a, b) -> case runParser (parseChar '.' *> parseMany (parseAnyChar ['0'..'9'])) b of
        Left (Missing(ar, br, c)) -> Left (Missing(ar, br, c ++ " -> parseFloat"))
        Right (c, d) -> Right (read (a ++ "." ++ c), d)