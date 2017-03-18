{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Maybe
import System.IO
import Control.Monad
import Data.List

split :: String -> [[String]]
split = reverse . tail . (map reverse) . (map (split' ',' [""])) . (split' '\n' [""])
        where
            split' :: Char -> [String] -> String -> [String]
            split' chr res [] = res
            split' chr (y:ys) (x:xs)
              | x == chr = split' chr ("":y:ys) xs
              | otherwise = split' chr ((y ++ [x]):ys) xs

data PolyType = PolyString String | PolyFloat Float | PolyInt Int | Invalid
instance Show PolyType where
  show (PolyString str) = str
  show (PolyInt num) = show num
  show (PolyFloat num) = show num
  show Invalid = "Invalid"

polyRead :: String -> String -> PolyType
polyRead "Int" str = case reads str :: [(Int,String)] of
                        [(x, "")] -> PolyInt x
                        _         -> Invalid
polyRead "String" str = case reads str :: [(String,String)] of
                           [(x, "")] -> PolyString x
                           _         -> Invalid
polyRead "Float" str = case reads str :: [(Float,String)] of
                           [(x, "")] -> PolyFloat x
                           _         -> Invalid
polyRead _ _ = Invalid

safeRead :: [[String]] -> ([String], [[PolyType]])
safeRead arr = (head arr, toTuples (head arr) $ tail arr)
                where
                    parse :: [String] -> [String] -> [PolyType]
                    parse [] [] = []
                    parse (x:xs) (y:ys) = (polyRead x y):(parse xs ys)
                    toTuples :: [String] -> [[String]] -> [[PolyType]]
                    toTuples types arr = map (parse types) arr

main = withFile "test.csv" ReadWriteMode (\handle -> do
           content <- hGetContents handle
           print $ safeRead $ split content)
