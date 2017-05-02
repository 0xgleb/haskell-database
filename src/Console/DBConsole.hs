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

toBinOp :: Ord a => String -> Maybe (a -> a -> Bool)
toBinOp "==" = Just (==)
toBinOp "<=" = Just (<=)
toBinOp ">=" = Just (>=)
toBinOp "<"  = Just (<)
toBinOp ">"  = Just (>)
toBinOp "/=" = Just (/=)
toBinOp _    = Nothing

eval :: [(String, AType)] -> String -> TT.Row -> Maybe PolyType
eval types str = polyToMaybe . (readSomething str <|>) . maybePolyToPoly . (safeHead <=< safeHead) . map TT.unwrap . TT.values . select [str] . TT.Table types . return
                 where
                     maybePolyToPoly (Just x) = x
                     maybePolyToPoly _        = Invalid
                     polyToMaybe Invalid = Nothing
                     polyToMaybe x       = Just x

getQuery :: DB -> [(String, String)] -> String -> IO (Maybe TT.Table)
getQuery db args target = ((composer args <*>) . return) <$> from db target
    where
        composer []                        = Just id
        composer (("select", '(':rest):xs) = liftM2 (.) (Just $ select (split ',' $ rm ' ' $ init rest)) (composer xs)
        composer (("select", arg):xs)      = liftM2 (.) (Just $ select [arg]) (composer xs)
        composer (("where", params):xs)    = let safeArgs = toTruple $ split ' ' $ init $ tail params in
                                                    case safeArgs of
                                                      Just (x, o, y) -> liftM2 (.) (composer xs) (where_ <$> ((\f t r -> maybeBoolToBool $ liftM2 f (eval t x r) (eval t y r)) <$> toBinOp o))
                                                      _              -> Nothing
        composer _ = Nothing

printMaybeTable :: Maybe TT.Table -> IO ()
printMaybeTable (Just table) = print table
printMaybeTable Nothing      = putStrLn "Invalid query!"

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
                                           "from" -> getQuery db (init tuples) (snd $ last tuples) >>= printMaybeTable >> next
                                           _      -> print (fst $ last tuples) >> putStrLn "Invalid query!" >> next
                        Nothing       -> putStrLn "Invalid query!" >> next
