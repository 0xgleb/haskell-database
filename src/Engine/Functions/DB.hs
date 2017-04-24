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

import System.Directory
import Control.Monad

import qualified Data.ByteString.Lazy as BL
import Data.Binary

toDBPath :: String -> FilePath
toDBPath = (++) "./.databases/"

create :: String -> IO DB
create name = createDirectoryIfMissing False (toDBPath name) >> return name

destroy :: DB -> IO Bool
destroy db = do
    exist <- doesDirectoryExist $ toDBPath db
    if exist then removeDirectoryRecursive (toDBPath db) >> return True
             else return False

ls :: IO [FilePath]
ls = listDirectory $ toDBPath ""

createTable :: DB -> TableName -> [(String, AType)] -> IO ()
createTable db table types = BL.writeFile (toPath db table) $ encode $ Table (types, [])

dropTable :: DB -> TableName -> IO Bool
dropTable db table = do
    let path = toPath db table
    exist <- doesFileExist path
    if exist then removeFile path >> return True
             else return False

listTables :: DB -> IO [String]
listTables db = map (join . init . split '.') <$> listDirectory (toDBPath db)
