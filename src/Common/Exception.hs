module Common.Exception 
( module Control.Exception
, Message
, catchT
, exceptionHandler
, printEitherT
, printException
) where

import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Exception

type Message = String

printEitherT :: Show a => EitherT Message IO a -> IO ()
printEitherT x = (showEither <$> runEitherT x) >>= putStrLn
    where showEither (Left message) = message
          showEither (Right result) = show result

printException :: EitherT Message IO a -> IO ()
printException x = (showEither <$> runEitherT x) >>= putStr
    where showEither (Left message) = message ++ "\n"
          showEither (Right result) = ""

catchT :: EitherT a IO b -> (SomeException -> EitherT a IO b) -> EitherT a IO b
catchT s handle = EitherT $ (runEitherT s) `catch` (runEitherT . handle)

exceptionHandler :: String -> String -> SomeException -> Message
exceptionHandler excp_module function = (++) ("An error occured in module " ++ excp_module ++ " in function " ++ function ++ ": ") . show
