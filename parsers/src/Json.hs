module Json ( JValue, json ) where

import Parser
    ( cr,
      lf,
      tab,
      space,
      dot,
      minus,
      oneOf,
      digit,
      exclude,
      char,
      string,
      dquotes,
      brackets,
      colon,
      comma,
      sepBy,
      braces,
      eof,
      Parser )
import Control.Applicative ( (<|>), Alternative (some, many) )
import Data.Map ( Map, fromList )

data JValue
    = JObject (Map String JValue)
    | JArray [JValue]
    | JString String
    | JFloat Float
    | JInt Int
    | JBool Bool
    | JNull
    deriving (Show, Eq)

-- Json McKeeman form

json :: Parser JValue
json = jElement <* eof

jValue :: Parser JValue
jValue = jObject <|> jArray <|> (JString <$> jString) <|> jFloat <|> jInt <|> jBool <|> jNull

jObject :: Parser JValue
jObject = JObject . fromList <$> braces (jMembers <* jWs)

jMembers :: Parser [(String, JValue)]
jMembers = jMember `sepBy` comma

jMember :: Parser (String, JValue)
jMember = do
    k <- jWs *> jString <* jWs
    v <- colon *> jElement
    return (k, v)

jArray :: Parser JValue
jArray = JArray <$> brackets (jElements <* jWs)

jElements :: Parser [JValue]
jElements = jElement `sepBy` comma

jElement :: Parser JValue
jElement = jWs *> jValue <* jWs

jString :: Parser String
jString = dquotes jCharacters

jCharacters :: Parser String
jCharacters = many jCharacter

jCharacter :: Parser Char
jCharacter = (string "\\" >>= const (char '"')) <|> exclude '"'

jFloat :: Parser JValue
jFloat = do
    i <- jInteger
    f <- jFraction
    e <- jExponent
    return $ JFloat (read (i ++ f ++ e))

jInt :: Parser JValue
jInt = do
    JInt . read <$> jInteger

jInteger :: Parser String
jInteger = (do { m <- minus;
                 ds <- do { d <- oneOf ['1'..'9']; ds <- jDigits; return (d:ds) } <|> ((:"") <$> digit);
                 return (m:ds) }) <|>
           (do { d <- oneOf ['1'..'9']; ds <- jDigits; return (d:ds) }) <|>
           ((:"") <$> digit)

jDigits :: Parser String
jDigits = some digit

jFraction :: Parser String
jFraction = (do { d <- dot; ds <- jDigits; return (d:ds) }) <|> return ""

jExponent :: Parser String
jExponent = (do { e <- oneOf ['e', 'E'];
                  ds <- (do {s <- jSign; ds <- jDigits; return (s:ds)}) <|> jDigits;
                  return (e:ds) }) <|>
            return ""

jSign :: Parser Char
jSign = char '+' <|> char '-'

jWs :: Parser String
jWs = many (space <|> tab <|> cr <|> lf)

jBool :: Parser JValue
jBool = JBool . (=="true") <$> (string "true" <|> string "false")

jNull :: Parser JValue
jNull = JNull <$ string "null"
