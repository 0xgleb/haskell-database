module Common.Exception
( module Control.Exception
, module Control.Monad.Trans.Except
, Message
, catchT
, exceptionHandler
, printExceptT
, printException
) where

import           Control.Exception
import           Control.Monad.Trans
import           Control.Monad.Trans.Except

type Message = String

printExceptT :: Show a => ExceptT Message IO a -> IO ()
printExceptT x = (showEither <$> runExceptT x) >>= putStrLn
    where showEither (Left message) = message
          showEither (Right result) = show result

printException :: ExceptT Message IO a -> IO ()
printException x = (showEither <$> runExceptT x) >>= putStr
    where showEither (Left message) = message ++ "\n"
          showEither (Right result) = ""

catchT :: ExceptT a IO b -> (SomeException -> ExceptT a IO b) -> ExceptT a IO b
catchT s handle = ExceptT $ (runExceptT s) `catch` (runExceptT . handle)

exceptionHandler :: String -> String -> SomeException -> Message
exceptionHandler excpModule function exception =
    "An error occured in module "
    ++ excpModule
    ++ " in function "
    ++ function
    ++ ": "
    ++ show exception
