module Engine.Types.Table.Table
( Row(..)
, unwrap
, Table(..)
, TableName
, types
, values
) where

import Data.Binary
import Data.Binary.Get (isEmpty)
import Data.List (foldl')

import Engine.Types.Table.PolyType

import Control.Monad.Extra (ifM)

newtype Row = Row [PolyType]
    deriving (Eq, Ord)

unwrap :: Row -> [PolyType]
unwrap (Row l) = l

instance Show Row where
    show (Row values) = show values

type TableName = String
newtype Table = Table ([(String, AType)], [Row])
    deriving Eq

types :: Table -> [(String, AType)]
types (Table (tableTypes, _)) = tableTypes

values :: Table -> [Row]
values (Table (_, tableValues)) = tableValues

instance Show Table where
    show (Table (tableTypes, tableValues)) = (show tableTypes) ++ (foldl' ((++) . (++ "\n")) "" (map show tableValues))

instance Binary Table where
    put (Table (tableTypes, tableValues)) = put tableTypes >> foldl (>>) mempty (foldl (>>) mempty $ map (map put) $ map unwrap tableValues)

    get = do tableTypes <- get :: Get [(String, AType)]
             tableValues <- getPolyTypes $ map snd tableTypes
             return $ Table (tableTypes, tableValues)
                 where
                     getPolyTypes tableTypes = ifM isEmpty (return []) (((:) <$> (fmap Row $ getRow tableTypes)) <*> getPolyTypes tableTypes)

                     getRow (x:xs) = ((:) <$> getPolyType x) <*> getRow xs
                     getRow [] = return []

                     getPolyType BoolType   = (get :: Get Bool)   >>= return . PolyBool
                     getPolyType IntType    = (get :: Get Int)    >>= return . PolyInt
                     getPolyType FloatType  = (get :: Get Float)  >>= return . PolyFloat
                     getPolyType StringType = (get :: Get String) >>= return . PolyString
