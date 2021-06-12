module EJson where


data JValue
    = JObject [(String, JValue)]
    | JArray [JValue]
    | JString String
    | JNumber Double
    | JBool Bool
    | JNull
    deriving (Show, Eq)

