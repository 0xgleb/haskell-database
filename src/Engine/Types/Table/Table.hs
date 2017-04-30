module Engine.Types.Table.Table
( Row(..)
, unwrap
, Table(..)
, TableName
, types
, values
, tableProduct
) where

import Data.Binary
import Data.Binary.Get (isEmpty)
import Data.List (foldl', transpose)

import Engine.Types.Table.PolyType

import Control.Monad (join)
import Control.Monad.Extra (ifM)

newtype Row = Row [PolyType]
    deriving (Eq, Ord)

unwrap :: Row -> [PolyType]
unwrap (Row l) = l

instance Show Row where
    show (Row values) = show values

type TableName = String
data Table = Table [(String, AType)] [Row]
    deriving Eq

types :: Table -> [(String, AType)]
types (Table tableTypes _) = tableTypes

values :: Table -> [Row]
values (Table _ tableValues) = tableValues

tableProduct :: [(String, Table)] -> Table
tableProduct tables = Table (join $ map (\(name, table) -> zip (map ((++) (name ++ ".") . fst) $ types table) (map snd $ types table)) tables) (map Row $ foldl (\xs ys -> [x ++ y | x <- xs, y <- ys]) [[]] $ map (map unwrap . values . snd) tables)

instance Show Table where
    show (Table tableTypes tableValues) = (show tableTypes) ++ (foldl' ((++) . (++ "\n")) "" (map show tableValues))

instance Binary Table where
    put (Table tableTypes tableValues) = put tableTypes >> foldl (>>) mempty (map (foldl (>>) mempty . map put . unwrap) tableValues)

    get = do tableTypes <- get :: Get [(String, AType)]
             tableValues <- getPolyTypes $ map snd tableTypes
             return $ Table tableTypes tableValues
                 where
                     getPolyTypes tableTypes = ifM isEmpty (return []) (((:) <$> (fmap Row $ getRow tableTypes)) <*> getPolyTypes tableTypes)

                     getRow (x:xs) = ((:) <$> getPolyType x) <*> getRow xs
                     getRow [] = return []

                     getPolyType BoolType   = (get :: Get Bool)   >>= return . PolyBool
                     getPolyType IntType    = (get :: Get Int)    >>= return . PolyInt
                     getPolyType FloatType  = (get :: Get Float)  >>= return . PolyFloat
                     getPolyType StringType = (get :: Get String) >>= return . PolyString
