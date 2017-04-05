module Parsing.String 
( parse
, areTypes
, safeRead
, readSomething
) where

import Common.String
import Types.PolyType
import Data.List (foldl')

parse :: [String] -> [String] -> [PolyType]
parse [] [] = []
parse (x:xs) (y:ys) = polyReadFromString x y : parse xs ys

areTypes :: [Maybe (String,String)] -> Bool
areTypes [] = True
areTypes ((Just (_, "Int")):xs) = areTypes xs
areTypes ((Just (_, "Float")):xs) = areTypes xs
areTypes ((Just (_, "String")):xs) = areTypes xs
areTypes (_:xs) = False

safeRead :: [[String]] -> ([(String, String)], [[PolyType]])
safeRead arr = ((map ((\[x1,x2] -> (x1, x2)) . split ':') . head) arr, 
                map (parse $ map (last . split ':') $ head arr) $ tail arr)

readSomething :: String -> PolyType
readSomething str = tmpReadSomething str possibleTypes
                    where 
                        tmpReadSomething str (x:xs) = polyReadFromString x str <|> tmpReadSomething str xs
                        tmpReadSomething _ [] = Invalid

polyReadFromString :: String -> String -> PolyType
polyRead "Int"    str = case reads str :: [(Int,String)] of
                                    [(x, "")] -> PolyInt x
                                    _         -> Invalid
polyRead "Float"  str = case reads str :: [(Float,String)] of
                                    [(x, "")] -> PolyFloat x
                                    _         -> Invalid
polyRead "String" str = case reads str :: [(String,String)] of
                                    [(x, "")] -> PolyString x
                                    _         -> Invalid
polyRead "Bool"   str = case reads str :: [(Bool,String)] of
                                    [(x, "")] -> PolyBool x
                                    _         -> Invalid
polyReadFromString _ _ = Invalid
