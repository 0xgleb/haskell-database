import Data.Maybe
import Data.List
import System.IO
import System.Directory
import Control.Monad

data PolyType = PolyString String | PolyFloat Float | PolyInt Int | Invalid
              deriving (Eq)
instance Show PolyType where
    show (PolyString str) = str
    show (PolyFloat int) = show int
    show (PolyInt float) = show float
    show Invalid = "Invalid"

polyRead :: String -> String -> PolyType
polyRead "Int" str = case reads str :: [(Int,String)] of
                        [(x, "")] -> PolyInt x
                        _         -> Invalid
polyRead "Float" str = case reads str :: [(Float,String)] of
                          [(x, "")] -> PolyFloat x
                          _         -> Invalid
polyRead "String" str = case reads str :: [(String,String)] of
                           [(x, "")] -> PolyString x
                           _         -> Invalid
polyRead _ _ = Invalid

format :: ([String], [[PolyType]]) -> String
-- format (types, values) = tail $ foldl' ((++) . (++ "\n")) "" $ 
-- map (tail . foldl' ((++) . (++ "\t")) "") $ types:(map (map show) values)
format (types, values) = (show types) ++ (foldl' ((++) . (++ "\n")) "" (map show values))

split :: Eq a => a -> [a] -> [[a]]
split target = reverse . splitHelper target [[]]
                where
                    splitHelper _ res [] = res
                    splitHelper chr (y:ys) (x:xs)
                      | x == chr = splitHelper chr ([]:y:ys) xs
                      | otherwise = splitHelper chr ((y ++ [x]):ys) xs

areTypes :: [String] -> Bool
areTypes [] = True
areTypes ("Int":xs) = areTypes xs
areTypes ("Float":xs) = areTypes xs
areTypes ("String":xs) = areTypes xs
areTypes (_:xs) = False

parse :: [String] -> [String] -> [PolyType]
parse [] [] = []
parse (x:xs) (y:ys) = (polyRead x y):(parse xs ys)

safeRead :: [[String]] -> ([String], [[PolyType]])
safeRead arr = (head arr, map (parse (head arr)) $ tail arr)

rm :: Eq a => a -> [a] -> [a]
rm target = filter (/= target)

toPath :: String -> FilePath
toPath = ("./databases/" ++) . (++ ".csv")

createDatabase :: String -> String -> IO ()
createDatabase name types = appendFile (toPath name) (types ++ "\n")
    >> putStrLn ("Database \"" ++ name ++ "\" with types " ++ types ++ " was successfully created")

readNewData :: String -> IO ()
readNewData name = do
    handle <- openFile (toPath name) ReadWriteMode
    content <- hGetContents handle
    let types = split ',' $ head $ lines content
    putStrLn ("Types: " ++ (head $ lines content))
    hClose handle
    putStr "Enter new data: "
    inputData <- getLine
    let newData = split ',' inputData
    if ([] == filter (== Invalid) (parse types newData))
       then appendFile (toPath name) $ inputData ++ "\n"
       else putStrLn "Invalid data"

workWithDatabase :: String -> IO ()
workWithDatabase name = do
    putStr (name ++ ":> ")
    command <- getLine
    case command of
        "read" -> do
            withFile (toPath name) ReadMode (\handle -> do
                content <- hGetContents handle
                putStrLn $ format $ safeRead $ map (split ',') $ lines content)
            workWithDatabase name
        "write" -> do
            readNewData name
            workWithDatabase name
        "exit" -> putStrLn "Exiting..."
        _      -> do
            putStrLn "Invalid command!"
            workWithDatabase name


main = do
    putStr "shell:> "
    command <- getLine
    case (head (split ' ' command)) of
          "create" -> do
              let args = tail $ split ' ' command
              fileExist <- doesFileExist $ toPath $ head args
              if fileExist then putStrLn "This database already exists!"
                  else if (areTypes $ split ',' $ rm ' ' $ join $ tail args)
                      then createDatabase (head args) $ join $ tail args
                      else putStrLn "Invalid types declaration!"
              main
          "drop" -> do
              let args = split ' ' command
              fileExist <- doesFileExist $ toPath $ head $ tail args
              if fileExist
                  then removeFile (toPath $ head $ tail args)
                    >> putStrLn ("Deleted database \"" ++ (head (tail args)) ++ "\"!")
                  else putStrLn "Selected database doesn't exist!"
              main
          "use" -> do
              let arg = head $ tail $ split ' ' command
              fileExist <- doesFileExist $ toPath arg
              if fileExist then workWithDatabase arg
                else putStrLn "Selected database doesn't exist!"
              main
          "list" -> do
              databases <- listDirectory "databases"
              print databases
              main
          "exit" -> do
              putStrLn "Exiting..."
          _      -> do
              putStrLn "Invalid command!"
              main
