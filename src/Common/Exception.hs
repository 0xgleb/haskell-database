module Common.Exception 
( module Control.Exception
, exceptionHandler
) where

import Control.Exception

exceptionHandler :: String -> String -> SomeException -> IO ()
exceptionHandler excp_module function = putStrLn . (++) ("An error occured in module " ++ excp_module ++ " in function " ++ function ++ ": ") . show
