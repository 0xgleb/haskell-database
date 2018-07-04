module Console.App
( console
) where

import           System.Directory
import           System.IO

import           Common.Exception
import           Control.Monad.Trans

import           Common.String
import           Console.DBConsole
import           Engine.Functions.DB

thisModule :: String
thisModule = "Console.App"

liftedConsole :: ExceptT Message IO ()
liftedConsole = lift console

consoleAction :: Bool -> IO () -> IO () -> ExceptT Message IO ()
consoleAction exists thenAction elseAction = do
    if exists then do lift thenAction
                      liftedConsole
              else do lift elseAction
                      liftedConsole

executeDBCommand :: [String] -> ExceptT Message IO ()
executeDBCommand args =
    case (length $ tail args) of
      0 -> case (head args) of
             "ls"   -> do lift (printExceptT listDBs)
                          liftedConsole
             "exit" -> lift $ putStrLn "Exiting..."
             ""     -> liftedConsole
             _      -> do lift (putStrLn "Invalid command!")
                          liftedConsole
      1 -> do
          let target = head $ tail args
          exists <- lift $ doesDirectoryExist $ toDBPath target
          case (head args) of
            "create"  ->
                consoleAction exists (putStrLn "Database already exists!")
                                     (printExceptT $ createDB target)
            "destroy" ->
                consoleAction exists (printExceptT $ dropDB target)
                                     (putStrLn "This database doesn't exist!")
            "ls" ->
                consoleAction exists (printExceptT $ listTables target)
                                     (putStrLn "This database doesn't exist!")
            "use" ->
                consoleAction exists (printException $ workWithDB target)
                                     (putStrLn "This database doesn't exist!")
            _ ->
                do lift (putStrLn "Invalid command!")
                   liftedConsole
      _ ->
          do lift (putStrLn "Invalid command!")
             liftedConsole


console :: IO ()
console = printException $
          ((lift $ putStr "db -> " >> hFlush stdout >> split ' ' <$> getLine) >>= executeDBCommand)
          `catchT` (throwE . exceptionHandler thisModule "console")
