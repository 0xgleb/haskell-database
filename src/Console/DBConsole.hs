module Console.DBConsole
( workWithDB
) where

import Engine.Functions.DB
import Engine.Functions.Table
import qualified Engine.Types.Table.Table as TT
import Engine.Types.Table.PolyType

import Data.Maybe (isJust)
import Common.Maybe
import Common.String
import Console.StringParsing

import Control.Monad
import System.IO
import System.IO.Error
import System.Directory

getQuery :: DB -> [(String, String)] -> [TT.TableName] -> IO (Maybe TT.Table)
getQuery db args targets = parseGetQuery args <$> from db targets

printMaybeTable :: Maybe TT.Table -> IO ()
printMaybeTable (Just table) = print table
printMaybeTable Nothing      = putStrLn "Invalid query!"

rmDuplicates :: Eq a => [a] -> [a]
rmDuplicates []     = []
rmDuplicates (x:xs) = x : rmDuplicates (filter (/= x) xs)

executeTableCommand :: DB -> [String] -> IO ()
executeTableCommand db args = let next = workWithDB db in 
                                  case (length $ tail args) of
                                    0 -> case (head args) of
                                           "ls"   -> listTables db >>= print >> next
                                           "exit" -> putStrLn "Exiting..."
                                           ""     -> next
                                           _      -> putStrLn "Invalid command!" >> next
                                    _ -> do
                                        let target = head $ tail args
                                        exist <- doesFileExist $ toPath db target
                                        case (head args) of
                                          "types"  -> if exist then tableTypes db target >>= print >> next else putStrLn "This table doesn't exist!" >> next
                                          "read"   -> if exist then from db [target]     >>= print >> next else putStrLn "This table doesn't exist!" >> next
                                          "drop"   -> if exist then dropTable db target            >> next else putStrLn "This table doesn't exist!" >> next
                                          "create" -> let safeTypes = map toType <$> (sequence $ map (toPair . split ':') $ split ',' $ join $ tail $ tail args) in
                                                          if exist 
                                                             then putStrLn "This table already exists!"
                                                             else case safeTypes of
                                                                    (Just types) -> if filter ((== InvalidType) . snd) types == [] then createTable db target types >> next
                                                                                                                                   else putStrLn "Invalid types!"   >> next
                                                                    _            -> putStrLn "Invalid types declaration!" >> next
                                          _ -> let safeArgs = sequence $ map (toPair . split '#') args in
                                                   case safeArgs of
                                                     (Just tuples) -> case (fst $ last tuples) of
                                                                        "from" -> getQuery db (init tuples) (rmDuplicates $ split ',' $ snd $ last tuples) >>= printMaybeTable >> next
                                                                        _      -> print (fst $ last tuples) >> putStrLn "Invalid query!" >> next
                                                     Nothing       -> putStrLn "Invalid query!" >> next

workWithDB :: DB -> IO ()
workWithDB db = (putStr (db ++ " => ") >> hFlush stdout >> split ' ' <$> getLine >>= executeTableCommand db) `catchIOError` (\_ -> putStrLn "An error occured while doing something...")
