{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module FFI where

import Foreign
import Foreign.C.Types
import Foreign.C.String


type CBool = CInt

fromCBool :: CBool -> Bool
fromCBool x
  | x == 0    = False
  | otherwise = True


foreign import ccall "GetServiceInfo" cGetServiceInfo :: CString    -> Ptr CString -> IO CInt
foreign import ccall "Initialize"     cInitialize     :: CString    -> CInt        -> IO CString
foreign import ccall "SetLogLevel"    cSetLogLevel    :: CInt       -> IO CString
foreign import ccall "SendCommand"    cSendCommand    :: CString    -> IO CString
foreign import ccall "FreeMemory"     cFreeMemory     :: CString    -> IO CBool
foreign import ccall "UnInitialize"   cUnInitialize   :: IO CString
--bool SetCallback(tcallback pCallback)
--bool SetCallbackEx(tcallbackEx pCallbackEx, void* userData)


getServiceInfo :: String -> IO (Either (Int, String) String)
getServiceInfo r = withCString r $
  \cstr -> alloca $
    \strptr -> do
      err <- fromIntegral <$> cGetServiceInfo cstr strptr
      str <- peek strptr
      res <- peekCString str
      freeMemory str
      if err == 0
        then return $ Right res
        else return $ Left (err, res)

initialize :: String -> Int -> IO (Maybe String)
initialize logPath logLevel = withCString logPath $
  \cstr -> do
      ret <- cInitialize cstr $ fromIntegral logLevel
      maybePeek peekCString ret

unInitialize :: IO (Maybe String)
unInitialize = do
  ret <- cUnInitialize
  maybePeek peekCString ret

freeMemory :: CString -> IO Bool
freeMemory x = fromCBool <$> cFreeMemory x