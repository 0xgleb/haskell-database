module Types.AType
( AType(..)
, possibleTypes
) where

import Data.Binary

possibleTypes :: [AType]
possibleTypes = [BoolType, IntType, FloatType, StringType]

data AType = BoolType | IntType | FloatType | StringType | InvalidType
             deriving (Show)

instance Binary AType where
    put InvalidType = putWord8 0
    put BoolType    = putWord8 1
    put IntType     = putWord8 2
    put FloatType   = putWord8 3
    put StringType  = putWord8 4

    get = do t <- getWord8
             case t of
               1 -> return $ BoolType
               2 -> return $ IntType
               3 -> return $ FloatType
               4 -> return $ StringType
               _ -> return $ InvalidType
