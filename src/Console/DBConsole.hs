module Console.DBConsole
( workWithDB
, rmDuplicates
) where


import           Common.Maybe
import           Common.String
import           Console.StringParsing
import           Control.Monad
import           Control.Monad.Trans
import           Data.Maybe             (isJust)
import           Safe
import           System.Directory
import           System.IO

import           Common.Exception
import           Engine.Functions.DB
import           Engine.Functions.Table

thisModule :: String
thisModule = "Console.DBConsole"

getQuery :: DBName -> [(String, String)] -> [TableName] -> ExceptT Message IO Table
getQuery db args targets = do
    ((maybeToExceptT . parseGetQuery args) =<< from db targets)
    `catchT` (throwE . exceptionHandler thisModule "getQuery")
    where maybeToExceptT Nothing  = throwE "Invalid query!"
          maybeToExceptT (Just x) = return x

printMaybeTable :: Maybe Table -> IO ()
printMaybeTable (Just table) = print table
printMaybeTable Nothing      = putStrLn "Invalid query!"

rmDuplicates :: Eq a => [a] -> [a]
rmDuplicates []     = []
rmDuplicates (x:xs) = x : rmDuplicates (filter (/= x) xs)

executeTableCommand :: DBName -> [String] -> IO ()
executeTableCommand db args =
    if (length $ tail args) == 0
    then case (head args) of
             "ls" -> do
                 printExceptT (listTables db)
                 next

             "exit" -> putStrLn "Exiting..."

             "" -> next

             _ -> do
                 putStrLn "Invalid query!"
                 next
    else do
        let target = head $ tail args
        exists <- doesFileExist $ toPath db target
        case (head args) of
          "types" ->
            consoleAction exists (printExceptT (getTableTypes db target))
                                 (putStrLn "This table doesn't exist!")
          "read" ->
            consoleAction exists (printExceptT (from db [target]))
                                 (putStrLn "This table doesn't exist!")
          "drop" ->
            consoleAction exists (printExceptT (dropTable db target))
                                 (putStrLn "This table doesn't exist!")
          "insert" ->
            consoleAction exists (printExceptT $ insert db target $ split ',' $ join $ drop 2 args)
                                 (putStrLn "This table doesn't exist!")
          "create" -> do
              let parameters = split '#' $ join $ drop 2 args
                  safeTypes = fmap (map toType) . traverse (toPair . split ':') . split ','
                            =<< headMay parameters
                  primKeys = maybe [] (split ',' . (\l -> take (length l - 1) l) . foldMap (++ "#"))
                           $ tailMay parameters

              if exists then putStrLn "This table already exists!"
                        else case safeTypes of
                            Nothing -> putStrLn "Invalid types declaration!"
                            (Just types) ->
                                if filter ((== InvalidType) . snd) types == []
                                    then printExceptT (createTable db target types primKeys)
                                    else putStrLn "Invalid types!"

              next
          _ -> do
              case traverse (toPair . split '#') args of
                  Nothing -> putStrLn "Invalid query!"
                  (Just tuples) -> case (fst $ last tuples) of
                      "from" -> do
                          let tableNames = rmDuplicates $ split ',' $ snd $ last tuples
                          exist <- all id <$> mapM (doesFileExist . toPath db) tableNames
                          consoleAction exist (printExceptT $ getQuery db (init tuples) tableNames)
                                              (putStrLn "One or more tables doesn't exist!")
                      _ -> do
                          putStrLn "Invalid query!"
                          next

    where next :: IO ()
          next = do runExceptT (workWithDB db)
                    pure ()

          consoleAction :: Bool -> IO () -> IO () -> IO ()
          consoleAction exists thenAction elseAction = do
              if exists then do thenAction
                                next
                        else do elseAction
                                next

workWithDB :: DBName -> ExceptT Message IO ()
workWithDB db =
    (lift $ do putStr (db ++ " => ")
               hFlush stdout
               split ' ' <$> getLine >>= executeTableCommand db
    ) `catchT` (throwE . exceptionHandler thisModule "workWithDB")
