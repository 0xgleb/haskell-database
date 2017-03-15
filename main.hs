import Data.Maybe
import System.IO
import Control.Monad
import Data.List

data PolyType = PolyString String | PolyFloat Float | PolyInt Int | Invalid
instance Show PolyType where
    show (PolyString str) = str
    show (PolyFloat int) = show int
    show (PolyInt float) = show float
    show Invalid = "Invalid"

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
polyRead _ _ = Invalid

format :: ([String], [[PolyType]]) -> String
-- format (types, values) = tail $ foldl' ((++) . (++ "\n")) "" $ 
-- map (tail . foldl' ((++) . (++ "\t")) "") $ types:(map (map show) values)
format (types, values) = (show types) ++ (foldl' ((++) . (++ "\n")) "" (map show values))

split :: Eq a => a -> [a] -> [[a]]
split target = reverse . splitHelper target [[]]
                where
                    splitHelper _ res [] = res
                    splitHelper chr (y:ys) (x:xs)
                      | x == chr = splitHelper chr ([]:y:ys) xs
                      | otherwise = splitHelper chr ((y ++ [x]):ys) xs

safeRead :: [[String]] -> ([String], [[PolyType]])
safeRead arr = (head arr, map (parse (head arr)) $ tail arr)
                where
                    parse [] [] = []
                    parse (x:xs) (y:ys) = (polyRead x y):(parse xs ys)

main = withFile "test.csv" ReadWriteMode (\handle -> do
           content <- hGetContents handle
           putStrLn $ format $ safeRead $ map (split ',') $ init $ split '\n' content)
