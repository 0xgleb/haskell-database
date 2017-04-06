import System.IO
import System.Directory
import System.Environment (getArgs)

import Control.Monad
import Control.Monad.Fix

import Control.Concurrent
import Control.Concurrent.Chan

import Network.Socket

import Common.String
import Parsing.String

import Interface.Database

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

runConn :: (Socket, SockAddr) -> Chan String -> IO ()
runConn (sock, _) chan = do
    let broadcast msg = writeChan chan msg
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    commLine <- dupChan chan
    readFile "client/index.html" >>= hPutStrLn hdl
    hClose hdl

serverLoop :: Socket -> Chan String -> IO ()
serverLoop sock chan = do
    conn <- accept sock
    forkIO $ runConn conn chan
    serverLoop sock chan

server :: IO ()
server = do
    putStrLn "Starting server."
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 3000 iNADDR_ANY)
    listen sock 2
    chan <- newChan
    putStrLn "Listening on port 3000."
    serverLoop sock chan

main :: IO ()
main = do 
    hSetBuffering stdin (BlockBuffering Nothing) 
    args <- getArgs
    if head args == "server" 
       then server
       else do 
           e <- doesDirectoryExist (toPath "") 
           if e then workWithDatabases
                else createDirectory (toPath "") >> workWithDatabases
