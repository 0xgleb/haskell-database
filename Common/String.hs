module Common.String
( rm
, split
) where

rm :: Eq a => a -> [a] -> [a]
rm target = filter (/= target)

split :: Char -> String -> [String]
split target = reverse . splitHelper target '_' [[]]
             where
                splitHelper _ _ res [] = res
                splitHelper chr '(' (y:ys) (x:xs)
                  | x == ')' = splitHelper chr '_' ((y ++ [x]):ys) xs
                  | otherwise = splitHelper chr '(' ((y ++ [x]):ys) xs
                splitHelper chr '"' (y:ys) (x:xs)
                  | x == '"' = splitHelper chr '_' ((y ++ [x]):ys) xs
                  | otherwise = splitHelper chr '"' ((y ++ [x]):ys) xs
                splitHelper chr '_' (y:ys) (x:xs)
                  | x == '(' = splitHelper chr '(' ((y ++ [x]):ys) xs
                  | x == '"' = splitHelper chr '"' ((y ++ [x]):ys) xs
                  | x == chr = splitHelper chr '_' ([]:y:ys) xs
                  | otherwise = splitHelper chr '_' ((y ++ [x]):ys) xs
