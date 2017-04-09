module Engine.Types.Table.Table
( Table(..)
, TableName
, types
, values
) where

import Data.Binary
import Data.List (foldl')

import Engine.Types.Table.PolyType

type TableName = String
newtype Table = Table ([(String, AType)], [[PolyType]])

types :: Table -> [(String, AType)]
types (Table (tableTypes, _)) = tableTypes

values :: Table -> [[PolyType]]
values (Table (_, tableValues)) = tableValues

instance Show Table where
    show (Table (tableTypes, tableValues)) = (show tableTypes) ++ (foldl' ((++) . (++ "\n")) "" (map show tableValues))

instance Binary Table where
    put (Table (tableTypes, tableValues)) = put tableTypes >> putAllData tableValues
                          where
                              putAllData (x:xs) = putWord8 1 >> (foldl' (>>) (return ()) $ map putPolyType x) >> putAllData xs
                              putAllData [] = putWord8 0
                              putPolyType Invalid           = put ()
                              putPolyType (PolyBool val)    = put val
                              putPolyType (PolyInt val)     = put val
                              putPolyType (PolyFloat val)   = put val
                              putPolyType (PolyString val)  = put val

                              
    get = do tableTypes <- get :: Get [(String, AType)]
             tableValues <- getPolyTypes $ map snd tableTypes
             return $ Table (tableTypes, tableValues)
                 where
                    getPolyTypes tableTypes = do t <- getWord8
                                                 case  t of
                                                   0 -> return []
                                                   1 -> ((:) <$> getRow tableTypes) <*> getPolyTypes tableTypes

                    getRow (x:xs) = ((:) <$> getPolyType x) <*> getRow xs
                    getRow [] = return []

                    getPolyType BoolType   = do val <- get :: Get Bool
                                                return $ PolyBool val
                    getPolyType IntType    = do val <- get :: Get Int
                                                return $ PolyInt val
                    getPolyType FloatType  = do val <- get :: Get Float
                                                return $ PolyFloat val
                    getPolyType StringType = do val <- get :: Get String
                                                return $ PolyString val

