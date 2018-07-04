module Engine.Types.Table.PolyType
( PolyType(..)
, (<|>)
, module Engine.Types.Table.AType
) where

import           Data.Binary
import           Engine.Types.Table.AType

data PolyType = PolyBool Bool | PolyInt Int | PolyFloat Float | PolyString String | Invalid
                deriving (Eq, Ord)

instance Show PolyType where
    show (PolyString val) = show val
    show (PolyFloat  val) = show val
    show (PolyInt    val) = show val
    show (PolyBool   val) = show val
    show Invalid          = "Invalid"

instance Binary PolyType where
    put Invalid          = put ()
    put (PolyBool   val) = put val
    put (PolyInt    val) = put val
    put (PolyFloat  val) = put val
    put (PolyString val) = put val

    get = undefined -- return Invalid

(<|>) :: PolyType -> PolyType -> PolyType
Invalid <|> x = x
x       <|> _ = x
