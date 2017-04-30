module Engine.Functions.Table
( module TT
, select
, tableTypes
, where_
, toPath
, from
, to
) where

import Engine.Types.DB
import qualified Engine.Types.Table as TT

import Control.Applicative
import Control.Monad
import Data.Maybe (maybeToList)
import Data.List (elemIndex)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import System.IO
import Data.Binary

toPath :: DB -> TT.TableName -> FilePath
toPath db = (("./.databases/" ++ db ++ "/") ++) . (++ ".table")

select :: [String] -> TT.Table -> Maybe TT.Table
select names (TT.Table fields values) = if length functionsList == length names then Just $ TT.Table (getElems fields) (map (TT.Row . getElems . TT.unwrap) values) else Nothing
    where 
        functionsList = join $ map (maybeToList . flip elemIndex (map fst fields)) names
        getElems list = foldl (\p f -> p ++ [f list]) [] $ map (flip (!!)) functionsList

where_ :: ([(String, TT.AType)] -> TT.Row -> Bool) -> TT.Table -> TT.Table
where_ f (TT.Table fields values) = TT.Table fields $ filter (f fields) values

from :: DB -> TT.TableName -> IO TT.Table
from db table = (decode :: BL.ByteString -> TT.Table) <$> BL.readFile (toPath db table)
    where
        rmDuplicates (x:xs) = x : rmDuplicates (filter (/= x) xs)

tableTypes :: DB -> TT.TableName -> IO [(String, TT.AType)]
tableTypes db table = TT.types <$> from db table

to :: DB -> TT.TableName -> TT.Row -> IO Bool
to db table newData = if filter (== TT.Invalid) (TT.unwrap newData) == []
                         then BL.appendFile (toPath db table) (BL.concat $ map encode $ TT.unwrap newData) >> return True
                         else return False
