module Console.App
( console
) where

import System.IO
import System.IO.Error
import System.Directory
import Control.Conditional

import Common.String
import Engine.Functions.DB
import Console.DBConsole

executeDBCommand :: [String] -> IO ()
executeDBCommand args = case (length $ tail args) of
                          0 -> case (head args) of
                                 "ls"   -> ls >>= putStrLn . show >> console
                                 "exit" -> putStrLn "Exiting..."
                                 ""     -> console
                                 _      -> putStrLn "Invalid command!" >> console
                          1 -> do
                              let target = head $ tail args
                              exist <- doesDirectoryExist $ toDBPath target
                              case (head args) of
                                "create"  -> if exist then putStrLn "Database already exists!" >> console else create target >> console
                                "destroy" -> if exist then destroy target                      >> console else putStrLn "This database doesn't exist!" >> console
                                "ls"      -> if exist then listTables target >>= print         >> console else putStrLn "This database doesn't exist!" >> console
                                "use"     -> if exist then workWithDB target                   >> console else putStrLn "This database doesn't exist!" >> console
                                _         -> putStrLn "Invalid command!" >> console
                          _ -> putStrLn "Invalid command!"

console :: IO ()
console = (putStr "db -> " >> hFlush stdout >> split ' ' <$> getLine >>= executeDBCommand) `catchIOError` (\_ -> putStrLn "An error occured while doing something..." >> console)
