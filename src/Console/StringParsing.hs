module Console.StringParsing
( polyRead
, parse
, readSomething
, toType
, eval
, toBinOp
, parseGetQuery
) where

import Common.String
import Common.Maybe
import Control.Monad
import Engine.Types.Table
import Engine.Functions.Table
import Data.List (foldl')

polyRead :: AType -> String -> PolyType
polyRead BoolType   str = case reads str :: [(Bool,String)] of
                                    [(x, "")] -> PolyBool x
                                    _         -> Invalid
polyRead IntType    str = case reads str :: [(Int,String)] of
                                    [(x, "")] -> PolyInt x
                                    _         -> Invalid
polyRead FloatType  str = case reads str :: [(Float,String)] of
                                    [(x, "")] -> PolyFloat x
                                    _         -> Invalid
polyRead StringType str = case reads str :: [(String,String)] of
                                    [(x, "")] -> PolyString x
                                    _         -> Invalid
polyRead _ _ = Invalid

parse :: [AType] -> [String] -> [PolyType]
parse = zipWith polyRead

toType :: (String, String) -> (String, AType)
toType (name, "Bool")   = (name, BoolType)
toType (name, "Int")    = (name, IntType)
toType (name, "Float")  = (name, FloatType)
toType (name, "String") = (name, StringType)
toType (name, _)        = (name, InvalidType)

readSomething :: String -> PolyType
readSomething str = tmpReadSomething str possibleTypes
                    where 
                        tmpReadSomething str (x:xs) = polyRead x str <|> tmpReadSomething str xs
                        tmpReadSomething _ [] = Invalid

toBinOp :: Ord a => String -> Maybe (a -> a -> Bool)
toBinOp "==" = Just (==)
toBinOp "<=" = Just (<=)
toBinOp ">=" = Just (>=)
toBinOp "<"  = Just (<)
toBinOp ">"  = Just (>)
toBinOp "/=" = Just (/=)
toBinOp _    = Nothing

eval :: [(String, AType)] -> String -> Row -> PolyType
eval types str = (readSomething str <|>) . maybePolyToPoly . (maybePolyToPoly . (safeHead <=< safeHead) . map unRow . values <$>) . select [str] . Table types . return
                 where maybePolyToPoly (Just x) = x
                       maybePolyToPoly _        = Invalid

parseGetQuery :: [(String, String)] -> Table -> Maybe Table
parseGetQuery []                        = return
parseGetQuery (("select", '(':rest):xs) = parseGetQuery xs >=> select (split ',' $ rm ' ' $ init rest)
parseGetQuery (("select", arg):xs)      = parseGetQuery xs >=> select [arg]
parseGetQuery (("where", params):xs)    = case toTrine $ split ' ' $ init $ tail params of
                                            Nothing        -> \_ -> Nothing
                                            Just (x, o, y) -> case (\f t r -> f (eval t x r) (eval t y r)) <$> toBinOp o of
                                                                Nothing -> \_ -> Nothing 
                                                                Just f  -> \table -> if (readSomething x /= Invalid || x `elem` map fst (types table)) 
                                                                                     && (readSomething y /= Invalid || y `elem` map fst (types table))
                                                                                              then (parseGetQuery xs >=> return . where_ f) table
                                                                                              else Nothing
parseGetQuery _ = \_ -> Nothing
