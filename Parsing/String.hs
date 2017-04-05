module Parsing.String 
( rm
, split
, parse
, areTypes
, safeRead
, readSomething
) where

import Types.PolyType
import Data.List (foldl')

rm :: Eq a => a -> [a] -> [a]
rm target = filter (/= target)

split :: Char -> String -> [String]
split target = reverse . splitHelper target '_' [[]]
             where
                splitHelper _ _ res [] = res
                splitHelper chr '(' (y:ys) (x:xs)
                  | x == ')' = splitHelper chr '_' ((y ++ [x]):ys) xs
                  | otherwise = splitHelper chr '(' ((y ++ [x]):ys) xs
                splitHelper chr '"' (y:ys) (x:xs)
                  | x == '"' = splitHelper chr '_' ((y ++ [x]):ys) xs
                  | otherwise = splitHelper chr '"' ((y ++ [x]):ys) xs
                splitHelper chr '_' (y:ys) (x:xs)
                  | x == '(' = splitHelper chr '(' ((y ++ [x]):ys) xs
                  | x == '"' = splitHelper chr '"' ((y ++ [x]):ys) xs
                  | x == chr = splitHelper chr '_' ([]:y:ys) xs
                  | otherwise = splitHelper chr '_' ((y ++ [x]):ys) xs

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
