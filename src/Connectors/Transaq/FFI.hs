{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Connectors.Transaq.FFI where


import Foreign
import Foreign.C.Types
import Foreign.C.String
import qualified Data.ByteString as B


type CBool = CInt

fromCBool :: CBool -> Bool
fromCBool x
  | x == 0    = False
  | otherwise = True

type TCallback = FunPtr (CString -> CBool)
type TCallbackEx a = FunPtr (CString -> Ptr a -> CBool)

foreign import ccall "wrapper" toTCallback :: (CString -> CBool) -> IO TCallback
foreign import ccall "wrapper" toTCallbackEx :: (CString -> Ptr a -> CBool) -> IO (TCallbackEx a)

foreign import ccall "GetServiceInfo" cGetServiceInfo :: CString       -> Ptr CString -> IO CInt
foreign import ccall "Initialize"     cInitialize     :: CString       -> CInt        -> IO CString
foreign import ccall "SetLogLevel"    cSetLogLevel    :: CInt          -> IO CString
foreign import ccall "SendCommand"    cSendCommand    :: CString       -> IO CString
foreign import ccall "FreeMemory"     cFreeMemory     :: CString       -> IO CBool
foreign import ccall "SetCallback"    cSetCallback    :: TCallback     -> IO CBool
foreign import ccall "SetCallbackEx"  cSetCallbackEx  :: TCallbackEx a -> Ptr a -> IO CBool
foreign import ccall "UnInitialize"   cUnInitialize   :: IO CString


getServiceInfo :: B.ByteString -> IO (Either (Int, B.ByteString) B.ByteString)
getServiceInfo r = B.useAsCString r $
  \cstr -> alloca $
    \strptr -> do
      err <- fromIntegral <$> cGetServiceInfo cstr strptr
      str <- peek strptr
      res <- B.packCString str
      freeMemory str
      if err == 0
        then return $ Right res
        else return $ Left (err, res)

initialize :: B.ByteString -> Int -> IO (Maybe B.ByteString)
initialize logPath logLevel = B.useAsCString logPath $
  \cstr -> do
    ret <- cInitialize cstr $ fromIntegral logLevel
    maybePeek B.packCString ret

unInitialize :: IO (Maybe B.ByteString)
unInitialize = do
  ret <- cUnInitialize
  maybePeek B.packCString ret

freeMemory :: CString -> IO Bool
freeMemory x = fromCBool <$> cFreeMemory x

sendCommand :: B.ByteString -> IO B.ByteString
sendCommand str = B.useAsCString str $
  \cstr -> do
    ret <- cSendCommand cstr
    res <- B.packCString ret
    freeMemory ret
    return res