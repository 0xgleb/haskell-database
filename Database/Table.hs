module Database.Table
( toPath
, workWithTable
) where

import Parser.String
import Parser.PolyType

import System.Directory
import System.IO

import Control.Applicative ((<*>))
import Control.Monad

import Data.Maybe (maybeToList)
import Data.List

toPath :: String -> String -> FilePath
toPath database = (("./.databases/" ++ database ++ "/") ++) . (++ ".csv")

toBinOp :: Ord a => String -> (a -> a -> Bool)
toBinOp "==" = (==)
toBinOp "/=" = (/=)
toBinOp ">" = (>)
toBinOp ">=" = (>=)
toBinOp "<" = (<)
toBinOp "<=" = (<=)

-- toOp :: Num a => String -> (a -> a -> a)
-- toOp "+" = (+)
-- toOp "-" = (-)
-- toOp "*" = (*)
-- toOp "/" = (/)

eval :: String -> [(String, String)] -> ([PolyType] -> PolyType)
eval str types rows = (readSomething str) <|> (head $ head $ select str types $ return $ rows)
-- eval ('@':xs) types = head . head . select xs types . return
-- eval str _ = \_ -> readSomething str
-- eval ('(':xs) types = split ' ' $ init xs

select :: String -> [(String, String)] -> ([[a]] -> [[a]])
select name types = return . (<*>) (maybeToList $ flip (!!) <$> (elemIndex name $ map fst types))

applyParams :: [[String]] -> [(String, String)] -> ([[PolyType]] -> [[PolyType]])
applyParams (("select":nm:[]):ys) types = select nm types . applyParams ys types
applyParams (("where":params:[]):ys) types = applyParams ys types . \l -> (\(x:o:y:[]) -> 
            filter (\el -> toBinOp o (eval x types el) (eval y types el)) l) $ split ' ' $ init $ tail params
applyParams [] _ = id

query :: [String] -> ([(String, String)], [[PolyType]]) -> [[PolyType]]
query params (types, cont) = applyParams (map (split '#') params) types cont

readData :: String -> String -> [String] -> IO ()
readData database name [] = withFile (toPath database name) ReadMode $ hGetContents >=> 
                                    putStrLn . format . safeRead . map (split ',') . lines
readData database name params = withFile (toPath database name) ReadMode $ hGetContents >=> 
                                    putStrLn . show . query params . safeRead . map (split ',') . lines

getNewData :: String -> String -> IO ()
getNewData database name = withFile (toPath database name) ReadWriteMode (\handle -> 
    hGetLine handle >>= (\types -> 
        (putStrLn . (++) "Types: ") types >> putStr "Enter new data: "
        >> getLine >>= (\inputData -> 
            if (==) [] $ filter (== Invalid) $ parse (map (last . split ':') $ split ',' types) $ split ',' inputData
               then hSeek handle SeekFromEnd 0 >> hPutStrLn handle inputData
               else putStrLn "Invalid data")))
    
workWithTable :: String -> String -> IO ()
workWithTable database name = do
    putStr $ database ++ "::" ++ name ++ " => "
    args <- split ' ' <$> getLine
    case (head args) of
        "read" -> do
            readData database name $ tail args
            workWithTable database name
        "write" -> getNewData database name >> workWithTable database name
        "exit" -> putStrLn "Exiting..."
        "" -> workWithTable database name
        _      -> putStrLn "Invalid command!" >> workWithTable database name
