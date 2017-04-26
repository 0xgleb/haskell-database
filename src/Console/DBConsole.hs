module Console.DBConsole
( eval
, toBinOp
, workWithDB
) where

import Engine.Functions.DB
import Engine.Functions.Table
import qualified Engine.Types.Table.Table as TT
import Engine.Types.Table.PolyType

import Common.Maybe
import Common.String
import Console.StringParsing

import Control.Monad
import System.IO
import System.Directory

toBinOp :: Ord a => String -> (a -> a -> Bool)
toBinOp "==" = (==)
toBinOp "<=" = (<=)
toBinOp ">=" = (>=)
toBinOp "<"  = (<)
toBinOp ">"  = (>)
toBinOp "/=" = (/=)
toBinOp _    = \_ _ -> False

eval :: [(String, AType)] -> String -> (TT.Row -> PolyType)
eval types str = (readSomething str <|>) . maybePolyToPoly . (safeHead >=> safeHead) . map TT.unwrap . select types [str] . return
                 where
                     maybePolyToPoly (Just x) = x
                     maybePolyToPoly _        = Invalid

getQuery :: DB -> [(String, String)] -> String -> IO [TT.Row]
getQuery db args target = (\table -> composer (TT.types table) args (TT.values table)) <$> from db target
    where
        composer types [] = id
        composer types (("select", '(':rest):xs) = select types (split ',' $ rm ' ' $ init rest) . composer types xs
        composer types (("select", arg):xs) = select types [arg] . composer types xs
        composer types (("where", params):xs) = let safeArgs = toTruple $ split ' ' $ init $ tail params in
                                                    case safeArgs of
                                                      Just (x, o, y) -> composer types xs . \l -> filter (\el -> toBinOp o (eval types x el) (eval types y el)) l
                                                      _              -> composer types xs . \l -> []
        composer _ _ = \_ -> []

workWithDB :: DB -> IO ()
workWithDB db = do
    let next = workWithDB db
    putStr $ db ++ " => "
    hFlush stdout
    args <- split ' ' <$> getLine
    case (length $ tail args) of
      0 -> case (head args) of
             "ls"   -> listTables db >>= print >> next
             "exit" -> putStrLn "Exiting..."
             ""     -> next
             _      -> putStrLn "Invalid command!" >> next
      _ -> do
           let target = head $ tail args
           exist <- doesFileExist $ toPath db target
           case (head args) of
             "types"  -> if exist then tableTypes db target >>= print >> next else putStrLn "This table doesn't exist!" >> next
             "read"   -> if exist then from db target       >>= print >> next else putStrLn "This table doesn't exist!" >> next
             "drop"   -> if exist then dropTable db target            >> next else putStrLn "This table doesn't exist!" >> next
             "create" -> do
                 let safeTypes = map toType <$> (sequence $ map (toTuple . split ':') $ split ',' $ join $ tail $ tail args) in
                     if exist 
                        then putStrLn "This table already exists!"
                        else case safeTypes of
                               (Just types) -> if filter ((== InvalidType) . snd) types == [] then createTable db target types >> next
                                                                                              else putStrLn "Invalid types!"   >> next
                               _            -> putStrLn "Invalid types declaration!" >> next
             _ -> let safeArgs = sequence $ map (toTuple . split '#') args in
                      case safeArgs of
                        (Just tuples) -> case (fst $ last tuples) of
                                           "from" -> getQuery db (init tuples) (snd $ last tuples) >>= putStrLn . show >> next
                                           _      -> print (fst $ last tuples) >> putStrLn "Invalid query!" >> next
                        Nothing       -> putStrLn "Invalid query!" >> next
