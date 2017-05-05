module Engine.Functions.DB
( module Engine.Types.DB
, create
, destroy
, toDBPath
, ls
, createTable
, dropTable
, listTables
) where

import Engine.Types.DB
import Engine.Types.Table
import Engine.Functions.Table
import Common.String

import System.IO
import System.IO.Error
import System.Directory
import Control.Monad
import Control.Monad.Extra

import qualified Data.ByteString.Lazy as BL
import Data.Binary

toDBPath :: String -> FilePath
toDBPath = (++) "./.databases/"

create :: String -> IO DB
create name = (createDirectoryIfMissing False (toDBPath name) >> return name) `catchIOError` (\_ -> putStrLn "An error occured while creating a database!" >> return "")

destroy :: DB -> IO Bool
destroy db = let dbPath = toDBPath db in catchIOError (ifM (doesDirectoryExist dbPath) (removeDirectoryRecursive dbPath >> return True) (return False)) 
                                                      (\_ -> putStrLn ("An error occured while destroying the database " ++ db ++ "!") >> return False)

ls :: IO [FilePath]
ls = listDirectory (toDBPath "") `catchIOError` (\_ -> putStrLn "An error occured while listing databases!" >> return [])

createTable :: DB -> TableName -> [(String, AType)] -> IO Bool
createTable db table types = let tablePath = toPath db table in catchIOError (ifM (doesFileExist tablePath) (return False) (BL.writeFile tablePath (encode $ Table types []) >> return True))
                                                                             (\_ -> putStrLn ("An error occured while creating a table " ++ table ++ "!") >> return False)

dropTable :: DB -> TableName -> IO Bool
dropTable db table = let path = toPath db table in catchIOError (ifM (doesFileExist path) (removeFile path >> return True) (return False))
                                                                (\_ -> putStrLn ("An error occured while droping the table " ++ table ++ "!") >> return False)

listTables :: DB -> IO [String]
listTables db = (map (join . init . split '.') <$> listDirectory (toDBPath db)) `catchIOError` (\_ -> putStrLn ("An error occured while listing tables in the db " ++ db ++ "!") >> return [])
