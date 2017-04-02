module Parser.PolyType 
( PolyType(..)
, polyRead
, (<|>)
, readSomething
) where

import Data.ByteString.Conversion

data PolyType = PolyString String | PolyFloat Float | PolyInt Int | PolyBool Bool | Invalid
    deriving (Eq, Ord)

instance Show PolyType where
    show (PolyString str) = show str
    show (PolyFloat int) = show int
    show (PolyInt float) = show float
    show Invalid = "Invalid"

instance ToByteString PolyType where
    builder (PolyString str) = builder str
    builder (PolyFloat int) = builder int
    builder (PolyInt float) = builder float
    builder Invalid = mempty

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
polyRead "Bool" str = case reads str :: [(Bool,String)] of
                            [(x, "")] -> PolyBool x
                            _         -> Invalid
polyRead _ _ = Invalid

(<|>) :: PolyType -> PolyType -> PolyType
Invalid <|> x = x
x <|> _ = x

readSomething :: String -> PolyType
readSomething str = polyRead "String" str <|> polyRead "Int" str <|> polyRead "Float" str
