module Types.Table
( Table(..)
, types
, values
) where

import Types.AType
import Types.PolyType
import Data.Binary
import Data.List (foldl')

newtype Table = Table ([(String, AType)], [[PolyType]])

types :: Table -> [(String, AType)]
types (Table (types, _)) = types

values :: Table -> [[PolyType]]
values (Table (_, values)) = values

instance Show Table where
    show (Table (types, values)) = (show types) ++ (foldl' ((++) . (++ "\n")) "" (map show values))

instance Binary Table where
    put (Table (types, values)) = put types >> putAllData values
                          where
                              putAllData (x:xs) = putWord8 1 >> (foldl' (>>) (return ()) $ map putPolyType x) >> putAllData xs
                              putAllData [] = putWord8 0
                              putPolyType Invalid           = put ()
                              putPolyType (PolyBool val)    = put val
                              putPolyType (PolyInt val)     = put val
                              putPolyType (PolyFloat val)   = put val
                              putPolyType (PolyString val)  = put val

                              
    get = do types <- get :: Get [(String, AType)]
             values <- getPolyTypes $ map snd types
             return $ Table (types, values)
                 where
                    getPolyTypes types = do t <- getWord8
                                            case t of
                                              0 -> return []
                                              1 -> ((:) <$> getRow types) <*> getPolyTypes types

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

