import System.IO
import System.Directory
import Control.Monad
import Database.Database
import Parser.String

toPath :: String -> FilePath
toPath = (++) "./.databases/"

main = do
    putStr "db -> "
    args <- split ' ' <$> getLine
    case (head args) of
        "create" -> do
            databaseExist <- doesDirectoryExist $ toPath $ head $ tail args
            if databaseExist then putStrLn "This database already exists!"
                             else createDirectory $ toPath $ head $ tail args
            main
        "destroy" -> do
            databaseExist <- doesDirectoryExist $ toPath $ head $ tail args
            if databaseExist then removeDirectoryRecursive (toPath $ head $ tail args)
                  >> putStrLn ("Deleted database \"" ++ (head $ tail args) ++ "\"!")
                else putStrLn "Selected database doesn't exist!"
            main
        "use" -> do
            databaseExist <- doesDirectoryExist $ toPath $ head $ tail args
            if databaseExist then workWithDatabase (head $ tail args)
                             else putStrLn "Selected database doesn't exist!"
            main
        "ls" -> listDirectory (toPath "") >>= print >> main
        "exit" -> putStrLn "Exiting..."
        _      -> putStrLn "Invalid command!" >> main
