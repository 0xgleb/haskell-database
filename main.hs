{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Maybe
import System.IO
import Control.Monad
import Data.List (foldl')

split :: String -> [[String]]
split = reverse . tail . (map reverse) . (map (split' ',' [""])) . (split' '\n' [""])
        where
            split' :: Char -> [String] -> String -> [String]
            split' chr res [] = res
            split' chr (y:ys) (x:xs)
              | x == chr = split' chr ("":y:ys) xs
              | otherwise = split' chr ((y ++ [x]):ys) xs

tuplemap :: (a -> b) -> (a, a, a) -> (b, b, b)
tuplemap f (x1, x2, x3) = (f x1, f x2, f x3)

safeRead :: [[String]] -> [Maybe (Maybe a1, Maybe a2, Maybe a3)]
safeRead = map (fmap (tuplemap maybeRead)) . map toTuple
            where
                toTuple [x1, x2, x3] = Just (x1, x2, x3)
                toTuple _ = Nothing
                maybeRead (str::Int) = case reads str [(Int,String)] of
                                            [(x, "")] -> Just x
                                            _         -> Nothing
                maybeRead (str::String) = case reads str [(String,String)] of
                                            [(x, "")] -> Just x
                                            _         -> Nothing

recomposeFunctors :: Eq a => [Maybe (Maybe a, Maybe a, Maybe a)] -> Maybe [(a,a,a)]
recomposeFunctors l
  | filter getNothings l == [] = Just (map (\(Just (Just x1, Just x2, Just x3)) -> (x1,x2,x3)) l)
  | otherwise = Nothing
            where
                getNothings (Just (Just _, Just _, Just _)) = True
                getNothings _ = False

main = withFile "test.dat" ReadWriteMode (\handle -> do 
           content <- hGetContents handle
           print $ recomposeFunctors . (safeRead) $ split content)
