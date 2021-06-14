{-# LANGUAGE TupleSections #-}

module Parser where

import Control.Applicative ( Alternative((<|>), empty, many) )
import Control.Monad ( replicateM )
import Data.List ( isPrefixOf )
import Data.Char ( isAlpha, isDigit, isAlphaNum )

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
                                            -- for now s2, TODO implement (Index, String) state

-- General parsing utilities
satisfy :: (Char -> Bool) -> ParserError -> Parser Char
satisfy f e = Parser (\s -> case s of
                            [] -> (Left e, s)
                            _  -> if f (head s)
                                  then (Right (head s), tail s)
                                  else (Left e, s))

oneOf :: [Char] -> Parser Char
oneOf list = satisfy (`elem` list) (show list)

noneOf :: [Char] -> Parser Char
noneOf list = satisfy (`notElem` list) ('!':show list)

count :: Int -> Parser a -> Parser [a]
count n p | n <= 0    = return []
          | otherwise = replicateM n p

exclude :: (Show a, Eq a) => Parser a -> [a] -> Parser a
exclude p ex = Parser (\s -> case parse p s of
                             (Right r, s') -> if r `elem` ex
                                              then (Left (show r ++ "!(" ++ show ex ++ ")"), s)
                                              else (Right r, s')
                             err           -> err)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> return []

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = do
    r  <- p
    rs <- many (sep >> p)
    return (r:rs)

endBy :: Parser a -> Parser sep -> Parser [a]
endBy p sep = many (p <* sep)

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = open *> p <* close

eof :: Parser ()
eof = Parser (\s -> case s of
                    [] -> (Right (), "")
                    _  -> (Left "EOF", s))

-- Specific patterns
char :: Char -> Parser Char
char x = satisfy (==x) [x]

anyChar :: Parser Char
anyChar = satisfy (const True) "anyChar"

notChar :: Char -> Parser Char
notChar x = satisfy (/=x) ('!':[x])

string :: String -> Parser String
string x = Parser (\s -> if x `isPrefixOf` s
                         then (Right x, drop (length x) s)
                         else (Left x, s))

notString :: String -> Parser String
notString x = Parser (\s -> let r = go x s
                            in (Right r, drop (length r) s))
    where go :: String -> String -> String
          go _ ""     = ""
          go "" s     = s
          go a (b:bs) = if a `isPrefixOf` (b:bs) then "" else b:go a bs

-- Specific characters and sequences
letter :: Parser Char
letter = satisfy isAlpha "letter"

digit :: Parser Char
digit = satisfy isDigit "digit"

alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum "alphaNum"

minus :: Parser Char
minus = char '-'

plus :: Parser Char
plus = char '+'

comma :: Parser Char
comma = char ','

dot :: Parser Char
dot = char '.'

colon :: Parser Char
colon = char ':'

semicolon :: Parser Char
semicolon = char ';'

space :: Parser Char
space = char ' '

tab :: Parser Char
tab = char '\t'

cr :: Parser Char
cr = char '\r'

lf :: Parser Char
lf = char '\n'

quote :: Parser Char
quote = char '\''

quotes :: Parser a -> Parser a
quotes = between quote quote

dquote :: Parser Char
dquote = char '"'

dquotes :: Parser a -> Parser a
dquotes = between dquote dquote

braces :: Parser a -> Parser a
braces = between (char '{') (char '}')

brackets :: Parser a -> Parser a
brackets = between (char '[') (char ']')

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')
