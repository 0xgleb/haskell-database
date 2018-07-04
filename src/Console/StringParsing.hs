module Console.StringParsing
( polyRead
, parse
, readSomething
, toType
, eval
, toBinOp
, parseGetQuery
) where

import           Control.Monad
import           Data.List              (foldl')
import           Safe

import           Common.Maybe
import           Common.String
import           Engine.Functions.Table
import           Engine.Types.Table

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
    where tmpReadSomething str (x:xs) = polyRead x str <|> tmpReadSomething str xs
          tmpReadSomething _   []     = Invalid

toBinOp :: Ord a => String -> Maybe (a -> a -> Bool)
toBinOp "==" = Just (==)
toBinOp "<=" = Just (<=)
toBinOp ">=" = Just (>=)
toBinOp "<"  = Just (<)
toBinOp ">"  = Just (>)
toBinOp "/=" = Just (/=)
toBinOp _    = Nothing

eval :: [(String, AType)] -> String -> Row -> PolyType
eval types str = (readSomething str <|>)
               . maybePolyToPoly
               . (maybePolyToPoly . (safeHead <=< safeHead) . map unRow . tableRows <$>)
               . select [str]
               . Table types []
               . pure
    where maybePolyToPoly (Just x) = x
          maybePolyToPoly _        = Invalid

parseGetQuery :: [(String, String)] -> Table -> Maybe Table
parseGetQuery [] = pure
parseGetQuery (("select", '(':rest):xs) =
    if last rest == ')' then parseGetQuery xs >=> select (split ',' $ rm ' ' $ init rest)
                        else const Nothing
parseGetQuery (("select", arg):xs) = parseGetQuery xs >=> select [arg]
parseGetQuery (("where", params):xs) =
    case toTrine $ split ' ' $ init $ tail params of
        Nothing        -> const Nothing
        Just (x, o, y) ->
            case (\f t r -> f (eval t x r) (eval t y r)) <$> toBinOp o of
                Nothing -> const Nothing
                Just f  ->
                    \table -> if (  readSomething x /= Invalid
                                 || x `elem` map fst (tableTypes table)
                                 ) &&
                                 (  readSomething y /= Invalid
                                 || y `elem` map fst (tableTypes table)
                                 )
                                 then (parseGetQuery xs >=> return . where_ f) table
                                 else Nothing
parseGetQuery _ = const Nothing
