{-# LANGUAGE ForeignFunctionInterface #-}

module Common.File
( update
, delete
) where

import           Common.Exception
import           Engine.Types.DB
import           Engine.Types.Table

import           Foreign.C.String
import           Foreign.C.Types

foreign import ccall safe "file_methods.c"
    c_update :: CString -> CInt -> IO ()

update :: DBName -> TableName -> Int -> Row -> ExceptT Message IO ()
update = undefined

foreign import ccall safe "file_methods.c"
    c_delete :: CString -> CInt -> CInt -> IO ()

delete :: DBName -> TableName -> Int -> ExceptT Message IO ()
delete = undefined
