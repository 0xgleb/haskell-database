module Database.Table
( toPath
, workWithTable
) where

import Parser.String
import Parser.PolyType
import System.Directory
import System.IO
import Control.Monad

toPath :: String -> String -> FilePath
toPath database = (("./.databases/" ++ database ++ "/") ++) . (++ ".csv")

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
    putStr (database ++ "::" ++ name ++ " => ")
    command <- getLine
    case command of
        "read" -> do
            withFile (toPath database name) ReadMode (\handle -> hGetContents handle 
                >>= (putStrLn . format . safeRead . map (split ',') . lines))
            workWithTable database name
        "write" -> getNewData database name >> workWithTable database name
        "exit" -> putStrLn "Exiting..."
        _      -> putStrLn "Invalid command!" >> workWithTable database name
