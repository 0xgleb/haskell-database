module Console.DBConsole
( workWithDB
) where

import Engine.Functions.DB
import Engine.Functions.Table

import Data.Maybe (isJust)
import Common.Maybe
import Common.String
import Console.StringParsing

import Control.Monad
import Control.Monad.Trans

import System.IO
import System.Directory
import Common.Exception

thisModule :: String
thisModule = "Console.DBConsole"

getQuery :: DBName -> [(String, String)] -> [TableName] -> ExceptT Message IO Table
getQuery db args targets = ((maybeToExceptT . parseGetQuery args) =<< from db targets) `catchT` (throwE . exceptionHandler thisModule "getQuery")
    where maybeToExceptT Nothing  = throwE "Invalid query!"
          maybeToExceptT (Just x) = return x

printMaybeTable :: Maybe Table -> IO ()
printMaybeTable (Just table) = print table
printMaybeTable Nothing      = putStrLn "Invalid query!"

rmDuplicates :: Eq a => [a] -> [a]
rmDuplicates []     = []
rmDuplicates (x:xs) = x : rmDuplicates (filter (/= x) xs)

executeTableCommand :: DBName -> [String] -> IO ()
executeTableCommand db args = let next = runExceptT (workWithDB db) >> return () in 
                                  case (length $ tail args) of
                                    0 -> case (head args) of
                                           "ls"   -> printExceptT (listTables db) >> next
                                           "exit" -> putStrLn "Exiting..."
                                           ""     -> next
                                           _      -> putStrLn "Invalid command!" >> next
                                    _ -> do
                                        let target = head $ tail args
                                        exist <- doesFileExist $ toPath db target
                                        case (head args) of
                                          "types"  -> if exist then printExceptT (tableTypes db target) >> next else putStrLn "This table doesn't exist!" >> next
                                          "read"   -> if exist then printExceptT (from db [target])     >> next else putStrLn "This table doesn't exist!" >> next
                                          "drop"   -> if exist then printExceptT (dropTable db target)  >> next else putStrLn "This table doesn't exist!" >> next
                                          "create" -> let safeTypes = map toType <$> (sequence $ map (toPair . split ':') $ split ',' $ join $ tail $ tail args) in
                                                          if exist 
                                                             then putStrLn "This table already exists!"
                                                             else case safeTypes of
                                                                    (Just types) -> if filter ((== InvalidType) . snd) types == [] then printExceptT (createTable db target types) >> next
                                                                                                                                   else putStrLn "Invalid types!" >> next
                                                                    _            -> putStrLn "Invalid types declaration!" >> next
                                          _ -> let safeArgs = sequence $ map (toPair . split '#') args in
                                                   case safeArgs of
                                                     (Just tuples) -> case (fst $ last tuples) of
                                                                        "from" -> printExceptT (getQuery db (init tuples) $ rmDuplicates $ split ',' $ snd $ last tuples) >> next
                                                                        _      -> putStrLn "Invalid query!" >> next
                                                     Nothing       -> putStrLn "Invalid query!" >> next

workWithDB :: DBName -> ExceptT Message IO ()
workWithDB db = (lift (putStr (db ++ " => ") >> hFlush stdout >> split ' ' <$> getLine >>= executeTableCommand db)) `catchT` (throwE . exceptionHandler thisModule "workWithDB")
