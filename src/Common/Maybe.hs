module Common.Maybe
( safeHead
, maybeBoolToBool
) where

safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead _      = Nothing

maybeBoolToBool :: Maybe Bool -> Bool
maybeBoolToBool Nothing  = False
maybeBoolToBool (Just b) = b
