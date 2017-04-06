module Parsing.String 
( parse
, areTypes
, readSomething
, toType
) where

import Common.String
import Types.PolyType
import Types.AType
import Data.List (foldl')

parse :: [AType] -> [String] -> [PolyType]
parse [] [] = []
parse (x:xs) (y:ys) = polyRead x y : parse xs ys

toType :: (String, String) -> (String, AType)
toType (name, "Bool")   = (name, BoolType)
toType (name, "Int")    = (name, IntType)
toType (name, "Float")  = (name, FloatType)
toType (name, "String") = (name, StringType)
toType (name, _)        = (name, InvalidType)

areTypes :: [[String]] -> Bool
areTypes [] = True
areTypes ((_:"Int":[]):xs) = areTypes xs
areTypes ((_:"Float":[]):xs) = areTypes xs
areTypes ((_:"String":[]):xs) = areTypes xs
areTypes _ = False

readSomething :: String -> PolyType
readSomething str = tmpReadSomething str possibleTypes
                    where 
                        tmpReadSomething str (x:xs) = polyRead x str <|> tmpReadSomething str xs
                        tmpReadSomething _ [] = Invalid

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
