module Engine.Functions.DB
( module Engine.Types.DB
, createDB
, dropDB
, toDBPath
, listDBs
, createTable
, dropTable
, listTables
) where

import           Common.String
import           Engine.Functions.Table
import           Engine.Types.DB
import           Engine.Types.Table

import           Control.Monad
import           Control.Monad.Extra
import           Control.Monad.Trans
import           System.Directory
import           System.IO

import           Common.Exception
import           Common.List

import           Data.Binary
import qualified Data.ByteString.Lazy   as BL

thisModule :: String
thisModule = "Engine.Functions.DB"

toDBPath :: String -> FilePath
toDBPath = (++) "./.databases/"

createDB :: DBName -> ExceptT Message IO DBName
createDB name = (lift (createDirectoryIfMissing False (toDBPath name)) >> pure name)
                `catchT` (throwE . exceptionHandler thisModule "create")

dropDB :: DBName -> ExceptT Message IO ()
dropDB db =
    (ifM (lift $ doesDirectoryExist path)
         (lift $ removeDirectoryRecursive path)
         (throwE $ "DB " ++ db ++ " doesn't exists!")
    ) `catchT` (throwE . exceptionHandler thisModule "destroyDB")
    where path = toDBPath db

listDBs :: ExceptT Message IO [DBName]
listDBs = (lift $ listDirectory (toDBPath "")) `catchT` (throwE . exceptionHandler thisModule "ls")

createTable :: DBName -> TableName -> [(String, AType)] -> [String] -> ExceptT Message IO ()
createTable db table types pKeys =
    (ifM (lift $ doesFileExist tablePath)
         (throwE $ "DB " ++ db ++ " already exists!")
         $ if all (flip elem $ map fst types) pKeys
              then lift $ BL.writeFile tablePath $ encode $ Table types pKeys []
              else throwE $ "Invalid types and/or primary keys list!"
                            ++ "\nTypes: " ++ show types
                            ++ "\nPrimary Keys: " ++ show pKeys
    ) `catchT` (throwE . exceptionHandler thisModule "createTable")
    where tablePath = toPath db table

dropTable :: DBName -> TableName -> ExceptT Message IO ()
dropTable db table =
    (ifM (lift $ doesFileExist path)
         (lift $ removeFile path)
         (throwE $ "Table " ++ table ++ " doesn't exist!")
    ) `catchT` (throwE . exceptionHandler thisModule "dropTable")
    where path = toPath db table

listTables :: DBName -> ExceptT Message IO [String]
listTables db = (lift $ map (join . init . split '.') <$> listDirectory (toDBPath db))
                `catchT` (throwE . exceptionHandler thisModule "listTables")
