module Engine.Types.Table.Table
( Row(..)
, Table(..)
, TableName
, tableProduct
, decodeTable
) where

import Data.Binary
import Data.Binary.Get (isEmpty)
import qualified Data.ByteString.Lazy as BL

import Engine.Types.Table.PolyType

import Control.Monad (join)
import Control.Monad.Extra (ifM)

newtype Row = Row { unRow :: [PolyType] } deriving (Eq, Ord)

instance Show Row where
    show (Row values) = show values

type TableName = String
data Table = Table { tableTypes  :: [(String, AType)]
                   , primaryKeys :: [String]
                   , tableValues :: [Row]
                   } deriving Eq

tableProduct :: [(String, Table)] -> Table
tableProduct tables = Table { tableTypes  = join $ map (\(name, table) -> zip (map ((++) (name ++ ".") . fst) $ tableTypes table) (map snd $ tableTypes table)) tables
                            , primaryKeys = join $ map (\(name, table) -> map ((++) $ name ++ ".") $ primaryKeys table) tables
                            , tableValues = map Row $ foldl (\xs ys -> [x ++ y | x <- xs, y <- ys]) [[]] $ map (map unRow . tableValues . snd) tables
                            }

decodeTable :: BL.ByteString -> Table
decodeTable = decode

instance Show Table where
    show (Table types pKeys values) = ("Primary keys: " ++ show pKeys ++ "\n") ++ (show types) ++ (foldl ((++) . (++ "\n")) "" $ map (show . unRow) values)

instance Binary Table where
    put (Table types pKeys values) = put types >> put pKeys >> foldl (>>) mempty (map (foldl (>>) mempty . map put . unRow) values)

    get = do types  <- get :: Get [(String, AType)]
             pKeys  <- get :: Get [String]
             values <- getPolyTypes $ map snd types
             return $ Table types pKeys values
                 where
                     getPolyTypes types = ifM isEmpty (return []) (((:) <$> (fmap Row $ getRow types)) <*> getPolyTypes types)

                     getRow (x:xs) = ((:) <$> getPolyType x) <*> getRow xs
                     getRow [] = return []

                     getPolyType BoolType   = (get :: Get Bool)   >>= return . PolyBool
                     getPolyType IntType    = (get :: Get Int)    >>= return . PolyInt
                     getPolyType FloatType  = (get :: Get Float)  >>= return . PolyFloat
                     getPolyType StringType = (get :: Get String) >>= return . PolyString
