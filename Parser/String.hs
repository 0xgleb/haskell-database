module Parser.String 
( module Parser.PolyType
, module Data.List
, rm
, format
, split
, parse
, areTypes
, safeRead
) where

import Parser.PolyType
import Data.List (foldl')

rm :: Eq a => a -> [a] -> [a]
rm target = filter (/= target)

format :: ([(String, String)], [[PolyType]]) -> String
format (types, values) = (show types) ++ (foldl' ((++) . (++ "\n")) "" (map show values))

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
parse (x:xs) (y:ys) = (polyRead x y):(parse xs ys)

areTypes :: [Maybe (String,String)] -> Bool
areTypes [] = True
areTypes ((Just (_, "Int")):xs) = areTypes xs
areTypes ((Just (_, "Float")):xs) = areTypes xs
areTypes ((Just (_, "String")):xs) = areTypes xs
areTypes (_:xs) = False

safeRead :: [[String]] -> ([(String, String)], [[PolyType]])
safeRead arr = ((map ((\[x1,x2] -> (x1, x2)) . split ':') . head) arr, 
                map (parse $ map (last . split ':') $ head arr) $ tail arr)
