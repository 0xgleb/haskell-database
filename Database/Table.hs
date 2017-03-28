module Database.Table
( toPath
, workWithTable
) where

import Parser.String
import Parser.PolyType

import System.Directory
import System.IO

import Control.Applicative ((<*>))
import Control.Monad

import Data.Maybe (maybeToList)
import Data.List

toPath :: String -> String -> FilePath
toPath database = (("./.databases/" ++ database ++ "/") ++) . (++ ".csv")

toBinOp :: Ord a => String -> (a -> a -> Bool)
toBinOp "==" = (==)
toBinOp "/=" = (/=)
toBinOp ">"  = (>)
toBinOp ">=" = (>=)
toBinOp "<"  = (<)
toBinOp "<=" = (<=)

-- toOp :: Num a => String -> (a -> a -> a)
-- toOp "+" = (+)
-- toOp "-" = (-)
-- toOp "*" = (*)
-- toOp "/" = (/)

eval :: String -> [(String, String)] -> ([PolyType] -> PolyType)
eval str types = (<|>) (readSomething str) . head . head . select str types . return

select :: String -> [(String, String)] -> ([[a]] -> [[a]])
select "*" _ = id
select ('(':xs) types = \l -> transpose $ join $ map (\str -> select str types l) $ split ',' $ rm ' ' $ init xs
select name types = return . (<*>) (maybeToList $ flip (!!) <$> (elemIndex name $ map fst types))

applyParams :: [[String]] -> [(String, String)] -> ([[PolyType]] -> [[PolyType]])
applyParams (("select":nm:[]):ys) types = select nm types . applyParams ys types
applyParams (("where":params:[]):ys) types = applyParams ys types . \l -> (\(x:o:y:[]) -> 
            filter (\el -> toBinOp o (eval x types el) (eval y types el)) l) $ split ' ' $ init $ tail params
applyParams [] _ = id

query :: [String] -> ([(String, String)], [[PolyType]]) -> [[PolyType]]
query params (types, cont) = applyParams (map (split '#') params) types cont

readTypes :: String -> String -> IO ()
readTypes database name = withFile (toPath database name) ReadMode $ hGetLine >=> putStrLn

readData :: String -> String -> [String] -> IO ()
readData database name [] = withFile (toPath database name) ReadMode $ hGetContents >=> 
                                    putStrLn . format . safeRead . map (split ',') . lines
readData database name params = withFile (toPath database name) ReadMode $ hGetContents >=> 
                                    putStrLn . show . query params . safeRead . map (split ',') . lines

getNewData :: String -> String -> IO ()
getNewData database name = withFile (toPath database name) ReadWriteMode $ \handle -> 
    hGetLine handle >>= (\types -> 
        (putStrLn . (++) "Types: ") types >> putStr "Enter new data: "
        >> getLine >>= (\inputData -> 
            if (==) [] $ filter (== Invalid) $ parse (map (last . split ':') $ split ',' types) $ split ',' inputData
               then hSeek handle SeekFromEnd 0 >> hPutStrLn handle inputData
               else putStrLn "Invalid data"))
    
workWithTable :: String -> String -> IO ()
workWithTable database name = do
    putStr $ database ++ "::" ++ name ++ " => "
    args <- split ' ' <$> getLine
    case (head args) of
      "types" -> readTypes database name >> workWithTable database name
      "read" -> readData database name (tail args) >> workWithTable database name
      "write" -> getNewData database name >> workWithTable database name
      "exit" -> putStrLn "Exiting..."
      "" -> workWithTable database name
      _      -> putStrLn "Invalid command!" >> workWithTable database name
