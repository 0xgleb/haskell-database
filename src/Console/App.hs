module Console.App
( console
) where

import System.IO
import System.Directory

import Control.Conditional
import Control.Monad.Trans
import Common.Exception

import Common.String
import Engine.Functions.DB
import Console.DBConsole

thisModule :: String
thisModule = "Console.App"

console :: IO ()
console = printException $ ((lift $ putStr "db -> " >> hFlush stdout >> split ' ' <$> getLine) >>= executeDBCommand) `catchT` (throwE . exceptionHandler thisModule "console")
    where
        liftedCli = lift console
        executeDBCommand args = case (length $ tail args) of
                                  0 -> case (head args) of
                                         "ls"   -> lift (printExceptT listDBs) >> liftedCli
                                         "exit" -> lift $ putStrLn "Exiting..."
                                         ""     -> liftedCli
                                         _      -> lift (putStrLn "Invalid command!") >> liftedCli
                                  1 -> do
                                      let target = head $ tail args
                                      exist <- lift $ doesDirectoryExist $ toDBPath target
                                      case (head args) of
                                        "create"  -> if exist then lift (putStrLn "Database already exists!") >> liftedCli else lift (printExceptT $ createDB target)            >> liftedCli
                                        "destroy" -> if exist then lift (printExceptT $ destroyDB target)       >> liftedCli else lift (putStrLn "This database doesn't exist!") >> liftedCli
                                        "ls"      -> if exist then lift (printExceptT $ listTables target)    >> liftedCli else lift (putStrLn "This database doesn't exist!") >> liftedCli
                                        "use"     -> if exist then lift (printException $ workWithDB target)  >> liftedCli else lift (putStrLn "This database doesn't exist!") >> liftedCli
                                        _         -> lift (putStrLn "Invalid command!") >> liftedCli
                                  _ -> lift (putStrLn "Invalid command!") >> liftedCli
