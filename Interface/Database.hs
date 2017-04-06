module Interface.Database
( workWithDatabase
) where

import Common.String
import Parsing.String

import Types.Table
import Interface.Table

import System.Directory
import Control.Monad

import qualified Data.ByteString.Lazy as BL
import Data.Binary

toTuple :: [a] -> (a, a)
toTuple (x:y:_) = (x, y)

createTable :: String -> String -> String -> IO ()
createTable database name types = BL.writeFile (toPath database name) $ encode (Table (map (toType . toTuple . split ':') (split ',' types), [[]]))

workWithDatabase :: String -> IO ()
workWithDatabase name = do
    putStr $ name ++ " => "
    args <- split ' ' <$> getLine
    if (head $ split '#' $ last args) == "from" 
       then do
           fileExist <- doesFileExist $ toPath name $ last $ split '#' $ last args
           if fileExist then workWithTable name (last $ split '#' $ last args) $ init args
                        else putStrLn "This table doesn't exist!"
           workWithDatabase name
       else case (head args) of
            "create" -> do
                fileExist <- doesFileExist $ toPath name $ head $ tail args
                if fileExist then putStrLn "This table already exists!"
                    else if areTypes $ map (split ':') $ split ',' $ last args
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
            "ls" -> listDirectory ("./.databases/" ++ name) 
                >>= print . map (join . init . split '.') >> (workWithDatabase name)
            "exit" -> putStrLn "Exiting..."
            "" -> workWithDatabase name
            _      -> putStrLn "Invalid command!" >> (workWithDatabase name)
