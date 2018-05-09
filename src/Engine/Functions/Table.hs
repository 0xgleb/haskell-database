module Engine.Functions.Table
( module Engine.Types.Table
, module Engine.Types.DB
, select
, getTableTypes
, where_
, toPath
, from
, searchTable
, search
, to
) where

import Engine.Types.DB
import Engine.Types.Table

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except

import Data.Maybe (maybeToList)
import Data.List (elemIndex)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import System.IO
import Data.Binary

import Common.Maybe
import Common.Exception

thisModule :: String
thisModule = "Engine.Functions.Table"

toPath :: DBName -> TableName -> FilePath
toPath db = (("./.databases/" ++ db ++ "/") ++) . (++ ".table")

select :: [String] -> Table -> Maybe Table
select names (Table types pKeys values) = toMaybe (length functionsList == length names) $ Table (getElems types) (filter (flip elem names) pKeys) (map (Row . getElems . unRow) values)
    where functionsList = join $ map (maybeToList . flip elemIndex (map fst types)) names
          getElems list = foldl (\p f -> p ++ [f list]) [] $ map (flip (!!)) functionsList

where_ :: ([(String, AType)] -> Row -> Bool) -> Table -> Table
where_ f (Table types pKeys values) = Table types pKeys $ filter (f types) values

from :: DBName -> [TableName] -> ExceptT Message IO Table
from db tables = (lift $ fmap (tableProduct . zipWith (,) tables) $ mapM (fmap decodeTable . BL.readFile . toPath db) tables) `catchT` (throwE . exceptionHandler thisModule "from")

getTableTypes :: DBName -> TableName -> ExceptT Message IO [(String, AType)]
getTableTypes db table = (fmap tableTypes $ from db [table]) `catchT` (throwE . exceptionHandler thisModule "tableTypes")

searchTable :: [PolyType] -> Table -> Maybe Row
searchTable pkValues table = find (\r -> ) $ tableRows table

search :: DBName -> TableName -> Row -> ExceptT Message (MaybeT IO) Row
search = undefined
-- search db table row = (lift $ MaybeT $ primKeysSearchTable row . decodeTable <$> (BL.readFile $ toPath db table)) `catchT` (throwE . exceptionHandler thisModule "search")

to :: DBName -> TableName -> Row -> ExceptT Message IO ()
to db table newData = if filter (== Invalid) (unRow newData) == []
                         then (lift $ BL.appendFile (toPath db table) (BL.concat $ map encode $ unRow newData)) `catchT` (throwE . exceptionHandler thisModule "to")
                         else throwE "Engine.Functions.Table.to: Cannot add invalid data!"
