module Xml where

import Parser
    ( Parser,
      oneOf,
      noneOf,
      sepBy,
      between,
      char,
      string,
      notString,
      letter,
      digit,
      space,
      tab,
      cr,
      lf,
      quotes,
      dquotes )
import Control.Applicative ( (<|>), Alternative (some, many) )
import Control.Monad ( void )
import Data.Map ( Map, fromList )

data XValue
    = XTag String (Map String String) [XValue]
    | XContent String
    deriving (Show, Eq)

-- Document
xml :: Parser XValue
xml = (xProlog <|> xMisc) *> xElement

-- Whitespace
xWs :: Parser String
xWs = some (space <|> tab <|> cr <|> lf)

-- Names and Tokens
xNameChar :: Parser Char
xNameChar = letter <|> digit <|> oneOf ".-_:"

xName :: Parser String
xName = do
    n <- letter <|> oneOf "_:'"
    ns <- many xNameChar
    return (n:ns)

-- Literals
xAttValue :: Parser String
xAttValue = dquotes (many (noneOf "%&\"")) <|> quotes (many (noneOf "%&'"))

-- -- Character Data
xCharData :: Parser String
xCharData = many (noneOf "<&")

-- Comments
xComment :: Parser String
xComment = between (string "<!--") (string "-->") (notString "--")

-- Prolog
xProlog :: Parser ()
xProlog = xMisc *> xXMLDecl <* xMisc

xXMLDecl :: Parser ()
xXMLDecl  = between (string "<?xml") (string "?>") (xVersionInfo <* many xWs)

xVersionInfo :: Parser ()
xVersionInfo = void $ xWs >> string "version" >> xEq >> (string "'1.0'" <|> string "\"1.0\"")

xEq :: Parser ()
xEq = void $ many xWs >> char '=' >> many xWs

xMisc :: Parser ()
xMisc = void $ many (xComment <|> xWs)

-- Elements, Tags and Element Content
xElement :: Parser XValue
xElement = xEmptyElemTag <|> (do
    (n, att) <- between (char '<') (char '>') xTag
    cont <- xContent
    _ <- between (string "</") (char '>') (string n) >> xMisc
    return $ XTag n (fromList att) cont)

xContent :: Parser [XValue]
xContent = do
    pre <- xMisc >> xCharData
    elems <- many (do
        markup <- xMisc *> xElement
        text <- xMisc >> xCharData
        return $ if not (null text) then markup:[XContent text] else [markup])
    return $ if not (null pre) then XContent pre:concat elems else concat elems

xTag :: Parser (String, [(String, String)])
xTag = do
    n <- xName
    att <- many xWs *> (xAttribute `sepBy` xWs) <* many xWs
    return (n, att)

xEmptyElemTag :: Parser XValue
xEmptyElemTag = do
    (n, att) <- between (char '<') (string "/>") xTag
    return $ XTag n (fromList att) []

xAttribute :: Parser (String, String)
xAttribute = do
    n <- xName <* xEq
    v <- xAttValue
    return (n, v)
