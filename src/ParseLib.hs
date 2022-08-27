{-# LANGUAGE LambdaCase #-}
module ParseLib where

import Control.Applicative

newtype Parser a = P (String -> Maybe (a,String))


parse :: Parser a -> String -> Maybe (a,String)
parse (P p) = p


instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    pa >>= f = P $ \str -> case parse pa str of
        Nothing        -> Nothing
        Just (a, str') -> parse (f a) str'

instance Functor Parser where
    -- fmap :: (a -> b) -> (Parser a -> Parser b)
    fmap g pa = do
        g <$> pa

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure a = P $ \str -> Just (a, str)

    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> pa = do
        f <- pf
        f <$> pa

instance Alternative Parser where
    -- empty :: Parser
    empty = P $ const Nothing

    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P $ \cs ->
        case parse p cs of
            Nothing -> parse q cs
            mx      -> mx


parseOne :: Parser Char
parseOne = P $ \case
    []     -> Nothing
    (c:cs) -> Just (c ,cs)


parseWhile :: (Char -> Bool) -> Parser String
parseWhile p = P $ \case
    [] -> Just ("", "")
    (c:cs)
        | p c -> case parse (parseWhile p) cs of
            Nothing           -> Just ([c], "")
            Just (rest, str') -> Just (c:rest, str')
        | otherwise -> Just ("", c:cs)



whitespace :: Parser ()
whitespace = do
    _ <- parseWhile (`elem` [' ', '\t', '\n'])
    return ()


number :: Parser Integer
number = do
    numstr <- parseWhile (`elem` "0123456789")
    let x = read numstr :: Integer
    return x


parseMany :: String -> Parser String
parseMany "" = do
    return ""
parseMany (c:cs) = do
    x <- parseOne
    if x == c then do
        xs <- parseMany cs
        return (x:xs)
    else empty


token :: String -> Parser String
token str = do
    _ <- whitespace
    tok <- parseMany str
    _ <- whitespace
    return tok


isInteger :: String -> Bool
isInteger = all (`elem` "0123456789")


word :: Parser String
word = do
    _ <- whitespace
    str <- parseWhile (`elem` ['a'..'z'])
    if str == "" then empty
    else do return str
