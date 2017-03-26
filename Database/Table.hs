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
toBinOp ">" = (>)
toBinOp ">=" = (>=)
toBinOp "<" = (<)
toBinOp "<=" = (<=)

-- eval :: String -> String -> String -> [String] -> ([[PolyType]] -> [[PolyType]])
-- eval ('@':x) op y types = filter $ (toOp op) () (polyRead (sec (types!!index)) y)
--                      where
--                          index = (transform (!!) . elemIndex types . map fst) (types:[])

select :: String -> [(String, String)] -> ([[PolyType]] -> [[PolyType]])
select name types = return . (<*>) (maybeToList $ flip (!!) <$> (elemIndex name $ map fst types))

applyParams :: [[String]] -> [(String, String)] -> ([[PolyType]] -> [[PolyType]])
applyParams (("select":nm:[]):ys) = select nm
-- applyParams (("where":nm:[]):ys) = (\(x:o:y:[]) -> eval x o y) . split ' ' . init . tail nm
applyParams [] = \_ -> id

query :: [String] -> ([(String, String)], [[PolyType]]) -> [[PolyType]]
query params (types, cont) = applyParams (map (split '#') params) types cont

readData :: String -> String -> [String] -> IO ()
readData database name [] = withFile (toPath database name) ReadMode $ hGetContents >=> 
                                    putStrLn . format . safeRead . map (split ',') . lines
readData database name params = withFile (toPath database name) ReadMode $ hGetContents >=> 
                                    putStrLn . show . query params . safeRead . map (split ',') . lines

getNewData :: String -> String -> IO ()
getNewData database name = withFile (toPath database name) ReadWriteMode (\handle -> 
    hGetLine handle >>= (\types -> 
        (putStrLn . (++) "Types: ") types >> putStr "Enter new data: "
        >> getLine >>= (\inputData -> 
            if (==) [] $ filter (== Invalid) $ parse (map (last . split ':') $ split ',' types) $ split ',' inputData
               then hSeek handle SeekFromEnd 0 >> hPutStrLn handle inputData
               else putStrLn "Invalid data")))
    
workWithTable :: String -> String -> IO ()
workWithTable database name = do
    putStr $ database ++ "::" ++ name ++ " => "
    args <- split ' ' <$> getLine
    case (head args) of
        "read" -> do
            readData database name $ tail args
            workWithTable database name
        "write" -> getNewData database name >> workWithTable database name
        "exit" -> putStrLn "Exiting..."
        _      -> putStrLn "Invalid command!" >> workWithTable database name
