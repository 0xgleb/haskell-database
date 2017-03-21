module Database.Table
( toPath
, workWithTable
, createTable
) where

import Parser.String
import Parser.PolyType
import System.Directory
import System.IO
import Control.Monad

toPath :: String -> String -> FilePath
toPath database = (("./.databases/" ++ database ++ "/") ++) . (++ ".csv")

createTable :: String -> String -> String -> IO ()
createTable database name types = appendFile (toPath database name) (types ++ "\n")
    >> putStrLn ("Table \"" ++ name ++ "\" with types (" 
    ++ types ++ ") was successfully created!")

readNewData :: String -> String -> IO ()
readNewData database name = do
    handle <- openFile (toPath database name) ReadWriteMode
    content <- hGetContents handle
    let types = split ',' $ head $ lines content
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
    putStr (name ++ "@" ++ database ++ ":> ")
    command <- getLine
    case command of
        "read" -> do
            withFile (toPath database name) ReadMode (\handle -> hGetContents handle 
                >>= (putStrLn . format . safeRead . map (split ',') . lines))
            workWithTable database name
        "write" -> readNewData database name >> workWithTable database name
        "exit" -> putStrLn "Exiting..."
        _      -> putStrLn "Invalid command!" >> workWithTable database name


