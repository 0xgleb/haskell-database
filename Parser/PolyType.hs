module Parser.PolyType 
( PolyType(..)
, polyRead
, readSomething
, fmap
) where

data PolyType = PolyString String | PolyFloat Float | PolyInt Int | Invalid
    deriving (Eq, Ord)
instance Show PolyType where
    show (PolyString str) = show str
    show (PolyFloat int) = show int
    show (PolyInt float) = show float
    show Invalid = "Invalid"

polyRead :: String -> String -> PolyType
polyRead "Int" str = case reads str :: [(Int,String)] of
                            [(x, "")] -> PolyInt x
                            _         -> Invalid
polyRead "Float" str = case reads str :: [(Float,String)] of
                            [(x, "")] -> PolyFloat x
                            _         -> Invalid
polyRead "String" str = case reads str :: [(String,String)] of
                            [(x, "")] -> PolyString x
                            _         -> Invalid
polyRead _ _ = Invalid

(<|>) :: PolyType -> PolyType -> PolyType
Invalid <|> x = x
x <|> _ = x

readSomething :: String -> PolyType
readSomething str = polyRead "String" str <|> polyRead "Int" str <|> polyRead "Float" str
