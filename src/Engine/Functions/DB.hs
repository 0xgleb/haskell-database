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
import System.Directory
import Control.Monad
import Control.Monad.Extra
import Common.Exception

import qualified Data.ByteString.Lazy as BL
import Data.Binary

thisModule :: String
thisModule = "Engine.Functions.DB"

toDBPath :: String -> FilePath
toDBPath = (++) "./.databases/"

create :: String -> IO DB
create name = (createDirectoryIfMissing False (toDBPath name) >> return name) `catch` (\e -> exceptionHandler thisModule "create" e >> return "")

destroy :: DB -> IO Bool
destroy db = (ifM (doesDirectoryExist dbPath) (removeDirectoryRecursive dbPath >> return True) (return False)) `catch` (\e -> exceptionHandler thisModule "destroy" e >> return False)
    where dbPath = toDBPath db

ls :: IO [FilePath]
ls = listDirectory (toDBPath "") `catch` (\e -> exceptionHandler thisModule "ls" e >> return [])

createTable :: DB -> TableName -> [(String, AType)] -> IO Bool
createTable db table types = let tablePath = toPath db table in catch (ifM (doesFileExist tablePath) (return False) (BL.writeFile tablePath (encode $ Table types []) >> return True))
                                                                      (\e -> exceptionHandler thisModule "createTable" e >> return False)

dropTable :: DB -> TableName -> IO Bool
dropTable db table = (ifM (doesFileExist path) (removeFile path >> return True) (return False)) `catch` (\e -> exceptionHandler thisModule "dropTable" e >> return False)
    where path = toPath db table

listTables :: DB -> IO [String]
listTables db = (map (join . init . split '.') <$> listDirectory (toDBPath db)) `catch` (\e -> exceptionHandler thisModule "listTables" e >> return [])
