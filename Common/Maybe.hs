module Common.Maybe
( safeHead
) where

safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead _      = Nothing
