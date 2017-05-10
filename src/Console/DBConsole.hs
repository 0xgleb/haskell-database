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
import Control.Monad.Trans.Either

import System.IO
import System.Directory
import Common.Exception

thisModule :: String
thisModule = "Console.DBConsole"

getQuery :: DB -> [(String, String)] -> [TableName] -> EitherT Message IO Table
getQuery db args targets = ((hoistEither . maybeToEither . parseGetQuery args) =<< from db targets) `catchT` (left . exceptionHandler thisModule "getQuery")
    where maybeToEither Nothing  = Left "Invalid query!"
          maybeToEither (Just x) = Right x

printMaybeTable :: Maybe Table -> IO ()
printMaybeTable (Just table) = print table
printMaybeTable Nothing      = putStrLn "Invalid query!"

rmDuplicates :: Eq a => [a] -> [a]
rmDuplicates []     = []
rmDuplicates (x:xs) = x : rmDuplicates (filter (/= x) xs)

executeTableCommand :: DB -> [String] -> IO ()
executeTableCommand db args = let next = runEitherT (workWithDB db) >> return () in 
                                  case (length $ tail args) of
                                    0 -> case (head args) of
                                           "ls"   -> printEitherT (listTables db) >> next
                                           "exit" -> putStrLn "Exiting..."
                                           ""     -> next
                                           _      -> putStrLn "Invalid command!" >> next
                                    _ -> do
                                        let target = head $ tail args
                                        exist <- doesFileExist $ toPath db target
                                        case (head args) of
                                          "types"  -> if exist then printEitherT (tableTypes db target) >> next else putStrLn "This table doesn't exist!" >> next
                                          "read"   -> if exist then printEitherT (from db [target])     >> next else putStrLn "This table doesn't exist!" >> next
                                          "drop"   -> if exist then printEitherT (dropTable db target)  >> next else putStrLn "This table doesn't exist!" >> next
                                          "create" -> let safeTypes = map toType <$> (sequence $ map (toPair . split ':') $ split ',' $ join $ tail $ tail args) in
                                                          if exist 
                                                             then putStrLn "This table already exists!"
                                                             else case safeTypes of
                                                                    (Just types) -> if filter ((== InvalidType) . snd) types == [] then printEitherT (createTable db target types) >> next
                                                                                                                                   else putStrLn "Invalid types!" >> next
                                                                    _            -> putStrLn "Invalid types declaration!" >> next
                                          _ -> let safeArgs = sequence $ map (toPair . split '#') args in
                                                   case safeArgs of
                                                     (Just tuples) -> case (fst $ last tuples) of
                                                                        "from" -> printEitherT (getQuery db (init tuples) $ rmDuplicates $ split ',' $ snd $ last tuples) >> next
                                                                        _      -> putStrLn "Invalid query!" >> next
                                                     Nothing       -> putStrLn "Invalid query!" >> next

workWithDB :: DB -> EitherT Message IO ()
workWithDB db = (lift (putStr (db ++ " => ") >> hFlush stdout >> split ' ' <$> getLine >>= executeTableCommand db)) `catchT` (left . exceptionHandler thisModule "workWithDB")
