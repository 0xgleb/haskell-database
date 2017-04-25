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

select :: [(String, TT.AType)] -> [String] -> ([TT.Row] -> [TT.Row])
select types names = map TT.Row . map (\row -> foldl (\p f -> p ++ [f row]) [] $ map (flip (!!)) $ join $ map (maybeToList . flip elemIndex (map fst types)) names) . map TT.unwrap

where_ :: (TT.Row -> Bool) -> ([TT.Row] -> [TT.Row])
where_ f = filter f

from :: DB -> TT.TableName -> IO TT.Table
from db table = (decode :: BL.ByteString -> TT.Table) <$> BL.readFile (toPath db table)

tableTypes :: DB -> TT.TableName -> IO [(String, TT.AType)]
tableTypes db table = TT.types <$> from db table

to :: DB -> TT.TableName -> TT.Row -> IO Bool
to db table newData = if filter (== TT.Invalid) (TT.unwrap newData) == []
                         then BL.appendFile (toPath db table) (BL.concat $ map encode $ TT.unwrap newData) >> return True
                         else return False
