module Types.PolyType
( PolyType(..)
, (<|>)
, module Types.AType
) where

import Types.AType

data PolyType = PolyBool Bool | PolyInt Int | PolyFloat Float | PolyString String | Invalid
                deriving (Eq, Ord)

instance Show PolyType where
    show (PolyString str) = show str
    show (PolyFloat int)  = show int
    show (PolyInt float)  = show float
    show Invalid          = "Invalid"

(<|>) :: PolyType -> PolyType -> PolyType
Invalid <|> x = x
x <|> _ = x