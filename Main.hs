import System.IO
import System.Directory
import System.Environment (getArgs)

import Server.Main
import Console.App

main :: IO ()
main = do 
    hSetBuffering stdin (BlockBuffering Nothing) 
    let databasesPath = ".databases"
    args <- getArgs
    if args == ["server"]
       then server
       else do 
           e <- doesDirectoryExist databasesPath
           if e then console
                else createDirectory databasesPath >> console
