module Common.List
( rmDuplicates
) where

rmDuplicates :: Eq a => [a] -> [a]
rmDuplicates []     = []
rmDuplicates (x:xs) = x : filter (/= x) xs
