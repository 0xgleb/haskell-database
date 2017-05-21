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
import Control.Monad.Trans.Either
import Common.Exception

import qualified Data.ByteString.Lazy as BL
import Data.Binary

thisModule :: String
thisModule = "Engine.Functions.DB"

toDBPath :: String -> FilePath
toDBPath = (++) "./.databases/"

createDB :: DBName -> EitherT Message IO DBName
createDB name = (lift (createDirectoryIfMissing False (toDBPath name)) >> right name) `catchT` (left . exceptionHandler thisModule "create")

destroyDB :: DBName -> EitherT Message IO ()
destroyDB db = (ifM (lift $ doesDirectoryExist path) (lift $ removeDirectoryRecursive path) (left $ "DB " ++ db ++ " doesn't exists!")) `catchT` (left . exceptionHandler thisModule "destroy")
    where path = toDBPath db

listDBs :: EitherT Message IO [DBName]
listDBs = (lift $ listDirectory (toDBPath "")) `catchT` (left . exceptionHandler thisModule "ls")

createTable :: DBName -> TableName -> [(String, AType)] -> EitherT Message IO ()
createTable db table types = catchT (ifM (lift $ doesFileExist tablePath) (left $ "DB " ++ db ++ " already exists!") (lift $ BL.writeFile tablePath $ encode $ Table types []))
                                    (left . exceptionHandler thisModule "createTable")
                                       where tablePath = toPath db table

dropTable :: DBName -> TableName -> EitherT Message IO ()
dropTable db table = (ifM (lift $ doesFileExist path) (lift $ removeFile path) (left $ "DB " ++ db ++ "doesn't exist!")) `catchT` (left . exceptionHandler thisModule "dropTable")
    where path = toPath db table

listTables :: DBName -> EitherT Message IO [String]
listTables db = (lift $ map (join . init . split '.') <$> listDirectory (toDBPath db)) `catchT` (left . exceptionHandler thisModule "listTables")
