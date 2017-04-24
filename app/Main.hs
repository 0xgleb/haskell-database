import System.IO
import System.Directory
import System.Environment (getArgs)

import Server
import Console

main :: IO ()
main = do 
    hSetBuffering stdin (BlockBuffering Nothing) 
    let databasesPath = ".databases"
    args <- getArgs
    e <- doesDirectoryExist databasesPath
    if e then return ()
         else createDirectory databasesPath
    if args == ["server"]
       then server
       else console
