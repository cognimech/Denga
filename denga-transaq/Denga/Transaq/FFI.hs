{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

-- denga, Haskell trading framework
-- Copyright (C) 2016 Leonid Vlasenkov <leo.vlasenkov@gmail.com>

-- | This module provides low-level interface for @txmlconnector@ library (<http://www.finam.ru/howtotrade/tconnector/>).
-- For more details on how to use these functions see official TXmlConnector user guide.

module Denga.Transaq.FFI
    ( getServiceInfo
    , initialize
    , unInitialize
    , sendCommand
    , setCallback
    , setCallbackEx
    , TCallback
    , TCallbackEx
    ) where



import           Foreign
import           Foreign.C.Types
import           Foreign.C.String
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8



type CBool = CInt

fromCBool :: CBool -> Bool
fromCBool x
    | x == 0    = False
    | otherwise = True

toCBool :: Bool -> CBool
toCBool x
    | x == True  = 1
    | x == False = 0

-- | Callback function pointer type for @SetCallback@ function
type TCallback = FunPtr (CString -> IO CBool)
-- | Callback function pointer type for @SetCallbackEx@ function
type TCallbackEx a = FunPtr (CString -> Ptr a -> IO CBool)



foreign import ccall "wrapper" toTCallback :: (CString -> IO CBool) -> IO TCallback
foreign import ccall "wrapper" toTCallbackEx :: (CString -> Ptr a -> IO CBool) -> IO (TCallbackEx a)

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
getServiceInfo :: B.ByteString -> IO (Either (Int, String) B.ByteString)
getServiceInfo r = B.useAsCString r $
    \cstr -> alloca $
        \strptr -> do
            err <- fromIntegral <$> cGetServiceInfo cstr strptr
            str <- peek strptr
            res <- B.packCString str
            cFreeMemory str
            if err == 0
                then return $ Right res
                else return $ Left (err, BC8.unpack res)

-- |Wrapper function for @Initialize@.
-- Returns Nothing on success and error message on error
initialize :: B.ByteString -> Int -> IO (Maybe String)
initialize logPath logLevel = B.useAsCString logPath $
    \cstr -> do
        ret <- cInitialize cstr $ fromIntegral logLevel
        maybePeekAndFree ret

-- |Wrapper function for @UnInitialize@.
-- Returns Nothing on success and error message on error
unInitialize :: IO (Maybe String)
unInitialize = do
    ret <- cUnInitialize
    maybePeekAndFree ret

-- |'maybePeek' analog. Frees memory if there is something to free.
maybePeekAndFree :: CString -> IO (Maybe String)
maybePeekAndFree ptr
    | ptr == nullPtr  = return Nothing
    | otherwise       = do
        a <- peekCString ptr
        cFreeMemory ptr
        return (Just a)

-- |Wrapper function for @SendCommand@.
sendCommand :: B.ByteString -> IO B.ByteString
sendCommand str = B.useAsCString str $
    \cstr -> do
        ret <- cSendCommand cstr
        res <- B.packCString ret
        cFreeMemory ret
        return res

-- |Wrapper function for @SetCallback@. Returns pointer to callback function on success and 'Nothing' on fail.
-- The pointer to callback should be freed manually using 'freeHaskellFunPtr' when the callback is not needed anymore.
setCallback :: (B.ByteString -> IO Bool) -> IO (Maybe TCallback)
setCallback f = do
    let
        g cs = do
            bs <- B.packCString cs
            cFreeMemory cs
            toCBool <$> f bs
    funptr <- toTCallback g
    retval <- cSetCallback funptr
    if retval == 0
        then do
            freeHaskellFunPtr funptr
            return Nothing
        else return $ Just funptr

-- |Wrapper function for @SetCallbackEx@. Returns pointer to callback function on success and 'Nothing' on fail.
-- The pointer to callback should be freed manually using 'freeHaskellFunPtr' when the callback is not needed anymore.
setCallbackEx :: (B.ByteString -> Ptr a -> IO Bool) -> Ptr a -> IO (Maybe (TCallbackEx a))
setCallbackEx f p = do
    let
        g cs ptr = do
            bs <- B.packCString cs
            cFreeMemory cs
            toCBool <$> f bs ptr
    funptr <- toTCallbackEx g
    retval <- cSetCallbackEx funptr p
    if retval == 0
        then do
            freeHaskellFunPtr funptr
            return Nothing
        else return $ Just funptr