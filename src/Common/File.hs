{-# LANGUAGE ForeignFunctionInterface #-}

module Common.File 
( update
, delete
) where

import Engine.Types.DB
import Engine.Types.Table
import Common.Exception

import Foreign.C.Types
import Foreign.C.String

foreign import ccall safe "file_methods.c"
    c_update :: CString -> CInt -> IO ()

update :: DBName -> TableName -> Int -> Row -> ExceptT Message IO ()
update = undefined

foreign import ccall safe "file_methods.c"
    c_delete :: CString -> CInt -> CInt -> IO ()

delete :: DBName -> TableName -> Int -> ExceptT Message IO ()
delete = undefined
