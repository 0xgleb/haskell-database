module Engine.Functions.Table
( module Engine.Types.Table
, module Engine.Types.DB
, select
, tableTypes
, where_
, toPath
, from
, to
) where

import Engine.Types.DB
import Engine.Types.Table

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except

import Data.Maybe (maybeToList)
import Data.List (elemIndex)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import System.IO
import Data.Binary

import Common.Exception

thisModule :: String
thisModule = "Engine.Functions.Table"

toPath :: DBName -> TableName -> FilePath
toPath db = (("./.databases/" ++ db ++ "/") ++) . (++ ".table")

select :: [String] -> Table -> Maybe Table
select names (Table fields values) = if length functionsList == length names then Just $ Table (getElems fields) (map (Row . getElems . unRow) values) else Nothing
    where functionsList = join $ map (maybeToList . flip elemIndex (map fst fields)) names
          getElems list = foldl (\p f -> p ++ [f list]) [] $ map (flip (!!)) functionsList

where_ :: ([(String, AType)] -> Row -> Bool) -> Table -> Table
where_ f (Table fields values) = Table fields $ filter (f fields) values

from :: DBName -> [TableName] -> ExceptT Message IO Table
from db tables = (lift $ fmap (tableProduct . zipWith (,) tables) $ mapM ((decodeTable <$>) . BL.readFile . toPath db) tables) `catchT` (throwE . exceptionHandler thisModule "from")

tableTypes :: DBName -> TableName -> ExceptT Message IO [(String, AType)]
tableTypes db table = (fmap types $ from db [table]) `catchT` (throwE . exceptionHandler thisModule "tableTypes")

to :: DBName -> TableName -> Row -> ExceptT Message IO ()
to db table newData = if filter (== Invalid) (unRow newData) == []
                         then (lift $ BL.appendFile (toPath db table) (BL.concat $ map encode $ unRow newData)) `catchT` (throwE . exceptionHandler thisModule "to")
                         else throwE "Engine.Functions.Table.to: Cannot add invalid data!"
