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

applyParams :: [[String]] -> [(String, String)] -> ([[PolyType]] -> [[PolyType]])
applyParams (("select":nm:[]):ys) = transform (!!) . elemIndex nm . map fst
applyParams [] = \_ -> id

query :: [String] -> ([(String, String)], [[PolyType]]) -> [[PolyType]]
query params (types, cont) = applyParams (map (split '#') params) types cont

-- (lines <$>) . hGetContents
readData :: String -> String -> [String] -> IO ()
readData database name [] = withFile (toPath database name) ReadMode (\handle -> hGetContents handle 
     >>= (putStrLn . format . safeRead . map (split ',') . lines))
readData database name params = withFile (toPath database name) ReadMode (\handle -> hGetContents handle 
     >>= (putStrLn . show . query params . safeRead . map (split ',') . lines))

getNewData :: String -> String -> IO ()
getNewData database name = do
    handle <- openFile (toPath database name) ReadWriteMode
    content <- hGetContents handle
    let types = map (last . split ':') $ split ',' $ head $ lines content
    putStrLn ("Types: " ++ (head $ lines content))
    hClose handle
    putStr "Enter new data: "
    inputData <- getLine
    let newData = split ',' inputData
    if ([] == filter (== Invalid) (parse types newData))
        then appendFile (toPath database name) $ inputData ++ "\n"
        else putStrLn "Invalid data"

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
