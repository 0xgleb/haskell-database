module Database.Table
( toPath
, workWithTable
) where

import Parser.String
import Parser.PolyType
import System.Directory
import System.IO
import Control.Monad
import Data.List

toPath :: String -> String -> FilePath
toPath database = (("./.databases/" ++ database ++ "/") ++) . (++ ".csv")

transform :: Monad m => ((m a) -> u -> b) -> Maybe u -> ([m a] -> m [b])
transform f (Just x) = return . map (\y -> f y x)
transform _ (Nothing) = \_ -> return []

toOp :: String -> (a -> b -> c)
toOp "==" = (==)
toOp ">" = (>)
toOp ">=" = (>=)
toOp "<" = (<)
toOp "<=" = (<=)

eval :: String -> String -> String -> [String] -> ([[PolyType]] -> [[PolyType]])
eval ('@':x) op y types = filter $ (toOp op) () (polyRead (sec (types!!index)) y)
                     where
                         index = (transform (!!) . elemIndex types . map fst) (types:[])

applyParams :: [[String]] -> [(String, String)] -> ([[PolyType]] -> [[PolyType]])
applyParams (("select":nm:[]):ys) = transform (!!) . elemIndex nm . map fst
applyParams (("where":nm:[]):ys) = (\(x:o:y:[]) -> eval x o y) . split ' ' . init . tail nm
applyParams [] = \_ -> id

query :: [String] -> ([(String, String)], [[PolyType]]) -> [[PolyType]]
query params (types, cont) = applyParams (map (split '#') params) types cont

readData :: String -> String -> [String] -> IO ()
readData database name [] = withFile (toPath database name) ReadMode (hGetContents >=>
    (putStrLn . format . safeRead . map (split ',') . lines))
readData database name params = withFile (toPath database name) ReadMode (hGetContents >=>
    (putStrLn . show . query params . safeRead . map (split ',') . lines))

getNewData :: String -> String -> IO ()
getNewData database name = withFile (toPath database name) ReadWriteMode (\handle -> do
        content <- hGetContents handle
        let types = map (last . split ':') $ split ',' $ head $ lines content
        putStrLn ("Types: " ++ (head $ lines content))
        putStr "Enter new data: "
        newData <- split ',' <$> getLine
        if ([] == filter (== Invalid) (parse types newData))
                then appendFile (toPath database name) $ inputData ++ "\n"
                else putStrLn "Invalid data")
    
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
