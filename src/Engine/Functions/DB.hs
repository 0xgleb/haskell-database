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
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Common.Exception

import qualified Data.ByteString.Lazy as BL
import Data.Binary

thisModule :: String
thisModule = "Engine.Functions.DB"

toDBPath :: String -> FilePath
toDBPath = (++) "./.databases/"

create :: String -> EitherT Message IO DB
create name = (lift (createDirectoryIfMissing False (toDBPath name)) >> right name) `catchT` (left . exceptionHandler thisModule "create")

destroy :: DB -> EitherT Message IO ()
destroy db = (ifM (lift $ doesDirectoryExist path) (lift $ removeDirectoryRecursive path) (left $ "DB " ++ db ++ " doesn't exists!")) `catchT` (left . exceptionHandler thisModule "destroy")
    where path = toDBPath db

ls :: EitherT Message IO [FilePath]
ls = (lift $ listDirectory (toDBPath "")) `catchT` (left . exceptionHandler thisModule "ls")

createTable :: DB -> TableName -> [(String, AType)] -> EitherT Message IO ()
createTable db table types = catchT (ifM (lift $ doesFileExist tablePath) (left $ "DB " ++ db ++ " already exists!") (lift $ BL.writeFile tablePath $ encode $ Table types []))
                                    (left . exceptionHandler thisModule "createTable")
                                       where tablePath = toPath db table

dropTable :: DB -> TableName -> EitherT Message IO ()
dropTable db table = (ifM (lift $ doesFileExist path) (lift $ removeFile path) (left $ "DB " ++ db ++ "doesn't exist!")) `catchT` (left . exceptionHandler thisModule "dropTable")
    where path = toPath db table

listTables :: DB -> EitherT Message IO [String]
listTables db = (lift $ map (join . init . split '.') <$> listDirectory (toDBPath db)) `catchT` (left . exceptionHandler thisModule "listTables")
