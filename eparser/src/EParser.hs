{-# LANGUAGE TupleSections #-}

module EParser where

import Control.Applicative ( Alternative((<|>), empty, some) )
import Data.List ( isPrefixOf )

type ParserError = String
newtype Parser a = Parser
    { parse :: String -> (Either ParserError a, String) }

instance Functor Parser where
    fmap f p = Parser (\s -> let (r, s') = parse p s
                             in (f <$> r, s'))

instance Applicative Parser where
    pure a = Parser (Right a,)
    pf <*> p = Parser (\s -> let (r , s' ) = parse pf s
                                 (r', s'') = parse p s'
                             in (r <*> r', s''))

instance Monad Parser where
    return = pure
    p >>= f = Parser (\s -> let (r, s') = parse p s
                            in case r of
                               Right r' -> parse (f r') s'
                               Left  r' -> (Left r', s'))

instance Alternative Parser where
    empty = Parser (Left "",)
    p1 <|> p2 = Parser (\s -> let (r1, s1) = parse p1 s
                                  (r2, s2) = parse p2 s
                              in case r1 of
                                 Right _ -> (r1, s1)
                                 Left e1 -> case r2 of
                                            Right _ -> (r2, s2)
                                            Left e2 -> (Left (e1 ++ "|" ++ e2), s2))

-- General parsing utilities
satisfy :: (Char -> Bool) -> ParserError -> Parser Char
satisfy f e = Parser (\(s:ss) -> if f s
                                 then (Right s, ss)
                                 else (Left e, s:ss))

try :: Parser a -> Parser a
try p = Parser (\s -> case parse p s of
                      (Left e, _)   -> (Left e, s)
                      (Right r, ss) -> (Right r, ss))

sepBy = undefined

between = undefined

-- Specific patterns
char :: Char -> Parser Char
char x = satisfy (==x) [x]

anyChar :: Parser Char
anyChar = satisfy (const True) []

string :: String -> Parser String
string x = Parser (\s -> if x `isPrefixOf` s
                         then (Right x, drop (length x) s)
                         else (Left x, s))

-- Specific characters and sequences
space :: Parser Char
space = char ' '

tab :: Parser Char
tab = char '\t'

newline :: Parser String
newline = some $ char '\n' <|> char '\r'

quote :: Parser Char
quote = char '"' <|> char '\''

eof :: Parser ()
eof = undefined
