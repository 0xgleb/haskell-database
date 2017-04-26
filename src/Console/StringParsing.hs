module Console.StringParsing
( polyRead
, parse
, readSomething
, toType
, toTuple
, toTruple
) where

import Common.String
import Engine.Types.Table.PolyType
import Engine.Types.Table.AType
import Data.List (foldl')

polyRead :: AType -> String -> PolyType
polyRead BoolType   str = case reads str :: [(Bool,String)] of
                                    [(x, "")] -> PolyBool x
                                    _         -> Invalid
polyRead IntType    str = case reads str :: [(Int,String)] of
                                    [(x, "")] -> PolyInt x
                                    _         -> Invalid
polyRead FloatType  str = case reads str :: [(Float,String)] of
                                    [(x, "")] -> PolyFloat x
                                    _         -> Invalid
polyRead StringType str = case reads str :: [(String,String)] of
                                    [(x, "")] -> PolyString x
                                    _         -> Invalid
polyRead _ _ = Invalid

parse :: [AType] -> [String] -> [PolyType]
parse = zipWith polyRead

toTuple :: [a] -> Maybe (a, a)
toTuple (x:y:[]) = Just (x, y)
toTuple _        = Nothing

toTruple :: [a] -> Maybe (a, a, a)
toTruple (x:y:z:[]) = Just (x, y, z)
toTruple _          = Nothing

toType :: (String, String) -> (String, AType)
toType (name, "Bool")   = (name, BoolType)
toType (name, "Int")    = (name, IntType)
toType (name, "Float")  = (name, FloatType)
toType (name, "String") = (name, StringType)
toType (name, _)        = (name, InvalidType)

readSomething :: String -> PolyType
readSomething str = tmpReadSomething str possibleTypes
                    where 
                        tmpReadSomething str (x:xs) = polyRead x str <|> tmpReadSomething str xs
                        tmpReadSomething _ [] = Invalid
