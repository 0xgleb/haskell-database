module Common.Maybe
( safeHead
, maybeBoolToBool
, toMaybe
, toPair
, toTrine
) where

safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead _      = Nothing

maybeBoolToBool :: Maybe Bool -> Bool
maybeBoolToBool Nothing  = False
maybeBoolToBool (Just b) = b

toMaybe :: Bool -> a -> Maybe a
toMaybe True  = Just
toMaybe False = const Nothing

toPair :: [a] -> Maybe (a, a)
toPair (x:y:[]) = Just (x, y)
toPair _        = Nothing

toTrine :: [a] -> Maybe (a, a, a)
toTrine (x:y:z:[]) = Just (x, y, z)
toTrine _          = Nothing
