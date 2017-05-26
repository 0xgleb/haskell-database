module Engine.Functions.DB
( module Engine.Types.DB
, createDB
, destroyDB
, toDBPath
, listDBs
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
import Control.Monad.Trans
import Common.Exception

import qualified Data.ByteString.Lazy as BL
import Data.Binary

thisModule :: String
thisModule = "Engine.Functions.DB"

toDBPath :: String -> FilePath
toDBPath = (++) "./.databases/"

createDB :: DBName -> ExceptT Message IO DBName
createDB name = (lift (createDirectoryIfMissing False (toDBPath name)) >> return name) `catchT` (throwE . exceptionHandler thisModule "create")

destroyDB :: DBName -> ExceptT Message IO ()
destroyDB db = let path = toDBPath db in catchT (ifM (lift $ doesDirectoryExist path) (lift $ removeDirectoryRecursive path) (throwE $ "DB " ++ db ++ " doesn't exists!")) 
                                                (throwE . exceptionHandler thisModule "destroyDB")

listDBs :: ExceptT Message IO [DBName]
listDBs = (lift $ listDirectory (toDBPath "")) `catchT` (throwE . exceptionHandler thisModule "ls")

createTable :: DBName -> TableName -> [(String, AType)] -> ExceptT Message IO ()
createTable db table types = catchT (ifM (lift $ doesFileExist tablePath) (throwE $ "DB " ++ db ++ " already exists!") (lift $ BL.writeFile tablePath $ encode $ Table types []))
                                    (throwE . exceptionHandler thisModule "createTable")
                                       where tablePath = toPath db table

dropTable :: DBName -> TableName -> ExceptT Message IO ()
dropTable db table = (ifM (lift $ doesFileExist path) (lift $ removeFile path) (throwE $ "DB " ++ db ++ "doesn't exist!")) `catchT` (throwE . exceptionHandler thisModule "dropTable")
    where path = toPath db table

listTables :: DBName -> ExceptT Message IO [String]
listTables db = (lift $ map (join . init . split '.') <$> listDirectory (toDBPath db)) `catchT` (throwE . exceptionHandler thisModule "listTables")
