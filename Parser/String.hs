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

format :: ([String], [[PolyType]]) -> String
format (types, values) = (show types) ++ (foldl' ((++) . (++ "\n")) "" (map show values))

split :: Eq a => a -> [a] -> [[a]]
split target = reverse . splitHelper target [[]]
                where
                    splitHelper _ res [] = res
                    splitHelper chr (y:ys) (x:xs)
                      | x == chr = splitHelper chr ([]:y:ys) xs
                      | otherwise = splitHelper chr ((y ++ [x]):ys) xs

parse :: [String] -> [String] -> [PolyType]
parse [] [] = []
parse (x:xs) (y:ys) = (polyRead x y):(parse xs ys)

areTypes :: [String] -> Bool
areTypes [] = True
areTypes ("Int":xs) = areTypes xs
areTypes ("Float":xs) = areTypes xs
areTypes ("String":xs) = areTypes xs
areTypes (_:xs) = False

safeRead :: [[String]] -> ([String], [[PolyType]])
safeRead arr = (head arr, map (parse (head arr)) $ tail arr)
