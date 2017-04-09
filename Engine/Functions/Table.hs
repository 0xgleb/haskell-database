module Engine.Functions.Table
( select
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
import Data.Binary

toPath :: DB -> TT.TableName -> FilePath
toPath db = (("./.databases/" ++ db ++ "/") ++) . (++ ".table")

select :: [(String, TT.AType)] -> [String] -> ([[a]] -> [[a]])
select types names = map (\row -> foldl (\p f -> p ++ [f row]) [] $ map (flip (!!)) $ join $ map (maybeToList . flip elemIndex (map fst types)) names)

where_ :: ([a] -> Bool) -> ([[a]] -> [[a]])
where_ f = filter f

from :: DB -> TT.TableName -> IO TT.Table
from db table = (decode :: BL.ByteString -> TT.Table) <$> BL.readFile (toPath db table)

tableTypes :: DB -> TT.TableName -> IO [(String, TT.AType)]
tableTypes db table = TT.types <$> from db table

to :: DB -> TT.TableName -> [TT.PolyType] -> IO Bool
to db table newData = do
    let path = toPath db table
    table <- BL.readFile path >>= return . decode :: IO TT.Table
    if filter (== TT.Invalid) newData == []
       then BL.writeFile path (encode $ TT.Table (TT.types table, TT.values table ++ [newData])) >> return True
       else return False
