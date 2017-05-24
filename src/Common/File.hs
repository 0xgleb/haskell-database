{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C.Types

foreign import ccall safe "file_methods.c"
    c_update :: CString -> CInt -> IO ()
    c_delete :: CString -> CInt -> CInt -> IO ()
