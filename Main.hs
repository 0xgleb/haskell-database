import System.IO
import System.Directory
import Control.Monad
import Interface.Database
import Parsing.String

toPath :: String -> FilePath
toPath = (++) "./.databases/"

workWithDatabases :: IO ()
workWithDatabases = do
    putStr "db -> "
    args <- split ' ' <$> getLine
    case (head args) of
        "create" -> do
            databaseExist <- doesDirectoryExist $ toPath $ head $ tail args
            if databaseExist then putStrLn "This database already exists!"
                             else createDirectory $ toPath $ head $ tail args
            workWithDatabases
        "destroy" -> do
            databaseExist <- doesDirectoryExist $ toPath $ head $ tail args
            if databaseExist then removeDirectoryRecursive (toPath $ head $ tail args)
                  >> putStrLn ("Deleted database \"" ++ (head $ tail args) ++ "\"!")
                else putStrLn "Selected database doesn't exist!"
            workWithDatabases
        "use" -> do
            databaseExist <- doesDirectoryExist $ toPath $ head $ tail args
            if databaseExist then workWithDatabase (head $ tail args)
                             else putStrLn "Selected database doesn't exist!"
            workWithDatabases
        "ls" -> listDirectory (toPath "") >>= print >> workWithDatabases
        "exit" -> putStrLn "Exiting..."
        "" -> workWithDatabases
        _      -> putStrLn "Invalid command!" >> workWithDatabases

main = hSetBuffering stdin (BlockBuffering Nothing) >> doesDirectoryExist (toPath "") >>= (\e -> if e
               then workWithDatabases
               else (createDirectory $ toPath "") >> workWithDatabases)
