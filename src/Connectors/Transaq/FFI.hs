{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

-- denga, Haskell trading framework
-- Copyright (C) 2016 Leonid Vlasenkov <leo.vlasenkov@gmail.com>

-- | This module provides low-level interface for @txmlconnector@ library (<http://www.finam.ru/howtotrade/tconnector/>).
-- For more details on how to use these functions see official TXmlConnector user guide.

module Connectors.Transaq.FFI (

  getServiceInfo,
  initialize,
  unInitialize,
  sendCommand

  ) where


import           Foreign
import           Foreign.C.Types
import           Foreign.C.String
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


-- |Wrapper function for @GetServiceInfo@.
-- Returns either tuple (error code, error message) or service info.
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

-- |Wrapper function for @Initialize@.
-- Returns Nothing on success and error message on error
initialize :: B.ByteString -> Int -> IO (Maybe B.ByteString)
initialize logPath logLevel = B.useAsCString logPath $
  \cstr -> do
    ret <- cInitialize cstr $ fromIntegral logLevel
    maybePeekAndFree ret

-- |Wrapper function for @UnInitialize@.
-- Returns Nothing on success and error message on error
unInitialize :: IO (Maybe B.ByteString)
unInitialize = do
  ret <- cUnInitialize
  maybePeekAndFree ret

-- |Wrapper function for @FreeMemory@.
freeMemory :: CString -> IO Bool
freeMemory x = fromCBool <$> cFreeMemory x

-- |'maybePeek' analog. Frees memory if there is something to free.
maybePeekAndFree :: CString -> IO (Maybe B.ByteString)
maybePeekAndFree ptr
  | ptr == nullPtr  = return Nothing
  | otherwise       = do
    a <- B.packCString ptr
    freeMemory ptr
    return (Just a)

-- |Wrapper function for @SendCommand@.
sendCommand :: B.ByteString -> IO B.ByteString
sendCommand str = B.useAsCString str $
  \cstr -> do
    ret <- cSendCommand cstr
    res <- B.packCString ret
    freeMemory ret
    return res
