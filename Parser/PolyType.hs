module Parser.PolyType 
( AType(..)
, PolyType(..)
, Table(..)
, polyReadFromString
, (<|>)
, readSomething
) where

import Data.Binary
import Data.Foldable (foldl')

possibleTypes :: [String]
possibleTypes = ["Bool", "Int", "Float", "String"]

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

data PolyType = PolyBool Bool | PolyInt Int | PolyFloat Float | PolyString String | Invalid
                deriving (Eq, Ord)

instance Show PolyType where
    show (PolyString str) = show str
    show (PolyFloat int)  = show int
    show (PolyInt float)  = show float
    show Invalid          = "Invalid"

putPolyType Invalid           = put ()
putPolyType (PolyBool val)    = put val
putPolyType (PolyInt val)     = put val
putPolyType (PolyFloat val)   = put val
putPolyType (PolyString val)  = put val

newtype Table = Table ([(String, AType)], [[PolyType]])
                deriving (Show)

instance Binary Table where
    put (Table (types, values)) = put types >> putAllData values
                          where
                              putAllData (x:xs) = putWord8 1 >> (foldl' (>>) (return ()) $ map putPolyType x) >> putAllData xs
                              putAllData [] = putWord8 0
                              
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

polyReadFromString :: String -> String -> PolyType
polyReadFromString "Int"    str = case reads str :: [(Int,String)] of
                                    [(x, "")] -> PolyInt x
                                    _         -> Invalid
polyReadFromString "Float"  str = case reads str :: [(Float,String)] of
                                    [(x, "")] -> PolyFloat x
                                    _         -> Invalid
polyReadFromString "String" str = case reads str :: [(String,String)] of
                                    [(x, "")] -> PolyString x
                                    _         -> Invalid
polyReadFromString "Bool"   str = case reads str :: [(Bool,String)] of
                                    [(x, "")] -> PolyBool x
                                    _         -> Invalid
polyReadFromString _ _ = Invalid

(<|>) :: PolyType -> PolyType -> PolyType
Invalid <|> x = x
x <|> _ = x

readSomething :: String -> PolyType
readSomething str = tmpReadSomething str possibleTypes
                    where 
                        tmpReadSomething str (x:xs) = polyReadFromString x str <|> tmpReadSomething str xs
                        tmpReadSomething _ [] = Invalid
