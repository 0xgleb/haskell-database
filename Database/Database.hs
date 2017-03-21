module Database.Database
( workWithDatabase
) where

import Parser.String
import Database.Table
import System.Directory
import Control.Monad

workWithDatabase :: String -> IO ()
workWithDatabase name = do
    putStr (name ++ ":> ")
    args <- split ' ' <$> getLine
    case (head args) of
        "create" -> do
            fileExist <- doesFileExist $ toPath name $ head $ tail args
            if fileExist then putStrLn "This table already exists!"
                else if (areTypes $ split ',' $ rm ' ' $ join $ tail args)
                    then createTable name (head $ tail args) $ join $ tail args
                    else putStrLn "Invalid types declaration!"
            workWithDatabase name
        "drop" -> do
            fileExist <- doesFileExist $ toPath name $ head $ tail args
            if fileExist
                then removeFile (toPath name $ head $ tail args)
                  >> putStrLn ("Deleted table \"" ++ (head (tail args)) ++ "\"!")
                else putStrLn "Selected table doesn't exist!"
            (workWithDatabase name)
        "use" -> do
            fileExist <- doesFileExist $ toPath name $ head $ tail args
            if fileExist then workWithTable name $ head $ tail args
              else putStrLn "Selected table doesn't exist!"
            (workWithDatabase name)
        "ls" -> listDirectory ("./.databases/" ++ name) 
            >>= print . map (join . init . split '.') >> (workWithDatabase name)
        "exit" -> putStrLn "Exiting..."
        _      -> putStrLn "Invalid command!" >> (workWithDatabase name)
