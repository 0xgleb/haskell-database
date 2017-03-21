module Database.Database
( workWithDatabase
) where

import Parser.String
import Database.Table
import System.Directory
import Control.Monad

toTuple :: [a] -> (a, a)
toTuple (x:y:[]) = (x, y)

createTable :: String -> String -> String -> IO ()
createTable database name types = do
    let valid = areTypes $ map (toTuple . split ':') $ split ',' types
    appendFile (toPath database name) (types ++ "\n") 
    >> putStrLn ("Table \"" ++ name ++ "\" with types (" ++ types ++ ") was created!")

workWithDatabase :: String -> IO ()
workWithDatabase name = do
    putStr $ name ++ " => "
    args <- split ' ' <$> getLine
    case (head args) of
        "create" -> do
            fileExist <- doesFileExist $ toPath name $ head $ tail args
            if fileExist then putStrLn "This table already exists!"
                         else if areTypes $ map (toTuple . split ':') $ split ',' $ last args
                            then createTable name (head $ tail args) $ join $ tail $ tail args
                            else putStrLn "Invalid declaration!"
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
