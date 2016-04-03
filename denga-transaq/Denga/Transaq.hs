{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

-- denga, Haskell trading framework
-- Copyright (C) 2016 Leonid Vlasenkov <leo.vlasenkov@gmail.com>

-- | 

module Denga.Transaq
    ( runTransaq
    , getServiceInfo
    , initialize
    , unInitialize
    , connect
    , disconnect
    , module Data.Default
    , module Denga.Transaq.Types
    ) where

-- remove it!
import           Data.Default (def)

import           Control.Monad.State
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Lens

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.UTF8 as UTF

import           Data.ByteString.Lex.Fractional
import qualified Data.Map as Map
import           Data.Maybe (
    fromJust
    )

import           Text.XML.Expat.Tree
import           Text.XML.Expat.Format
import           Text.XML.Expat.Proc

import           Foreign (
    freeHaskellFunPtr
    )

import           Denga.Core
import           Denga.Transaq.Types
import qualified Denga.Transaq.FFI as FFI

-----------------------------------------------------------------------------------------------------------------------------

-- | Launch your algorithm.
runTransaq :: Transaq a -- ^ Algorithm. Sequence of actions inside Transaq monad.
           -> Settings  -- ^ Transaq settings specified by user. 'def' may be used.
           -> IO a      -- ^ Return value of the algorithm.
runTransaq algo s = evalStateT algo $ def { _settings = s }

-- | Send @connect@ command with default (previously specified) settings.
-- Return 'Nothing' on succes or a message on fail.
connect :: Transaq (Maybe String)
connect = do
  sts <- use settings
  let xml = connectionToXML $ connection sts
  liftIO $ sendCommand $ formatNode' xml

-- | Send @disconnect@ command.
-- Return 'Nothing' on succes or a message on fail.
disconnect :: Transaq (Maybe String)
disconnect = do
  liftIO $ sendCommand "<command id=\"disconnect\"/>"

-- | Initialize Transaq Connector.
-- Return 'Nothing' on succes or a message on fail.
initialize :: Transaq (Maybe String)
initialize = do
    sts <- use settings
    liftIO $ FFI.initialize (BC8.pack $ logDirectory sts) (logLevel sts)

-- | Uninitialize Transaq Connector.
-- Return 'Nothing' on succes or a message on fail.
unInitialize :: Transaq (Maybe String)
unInitialize = liftIO $ FFI.unInitialize

-- | Get service info.
-- Return either tuple (error code, message) or 'ServiceInfo'.
getServiceInfo :: Transaq (Either (Int, String) ServiceInfo)
getServiceInfo = do
    result <- liftIO $ FFI.getServiceInfo
        "<request><value>queue_size</value><value>queue_mem_used</value><value>version</value></request>"
    case result of
        Left err -> return $ Left err
        Right bs -> handleXMLData bs $ \xml -> return $ Right $ ServiceInfo
            { serInfQueueSize = fromJust $ getChildText "queue_size" xml
            , serInfQueueMemUsed = fromJust $ getChildText "queue_mem_used" xml
            , serInfVersion = fromJust $ getChildText "version" xml
            }

-----------------------------------------------------------------------------------------------------------------------------

-- | Send specified command.
-- Return 'Nothing' on succes or a message on fail.
sendCommand :: BString -> IO (Maybe String)
sendCommand command = do
    result <- FFI.sendCommand command
    return $ handleXMLData result $ \xml -> case xml of
        Element "result" [("success", "true")]  _                                -> Nothing
        Element "result" [("success", "false")] [Element "message" _ [Text msg]] -> Just $ BC8.unpack msg
        Element "error"  _                      [Text msg]                   -> throw $ SendCommandException $ UTF.toString  msg
        otherwise -> throw $ XMLParserException "illegal attributes for <result> element"

-- | Make single callback function to pass to 'FFI.setCallback' by partial appliance to a 'Map.Map'
-- of separate user-specified callbacks.
mkCallback :: Map.Map BString (XML -> IO Bool) -> BString -> IO Bool
mkCallback cbmap bs = handleXMLData bs $ \xml -> case Map.lookup (eName xml) cbmap of
    Just cb -> cb xml
    Nothing -> return True

-- | Reset callback function in accordance with current @callbacks@ field of the state.
updateCallback :: Transaq Bool
updateCallback = do
    m <- use callbacks
    mbcb <- liftIO $ FFI.setCallback $ mkCallback m
    case mbcb of
        Nothing -> return False
        Just _  -> do
            cb <- use callbackFunPtr
            liftIO $ maybe (return ()) freeHaskellFunPtr cb
            callbackFunPtr .= mbcb
            return True

-----------------------------------------------------------------------------------------------------------------------------

instance DataSource Transaq where
    onTick f = do
        let
            g ts = do
                bs <- sequence $ map (f.xmlToTick) $ eChildren ts
                return $ foldl (&&) True bs
        callbacks %= Map.insert "ticks" g
        updateCallback

-----------------------------------------------------------------------------------------------------------------------------

getChildText :: BString -> XML -> Maybe BString
getChildText nm xml = do
    c <- findChild nm xml
    case onlyText $ eChildren c of
        []    -> Nothing
        (x:_) -> Just x

-- | Translate @<tick>@ XML element to 'Tick' 
xmlToTick :: XML -> Tick
xmlToTick xml =
    let
        Just price = getChildText "price" xml
        Just (pr, str) = readDecimal price
    in  Tick {tickPrice = pr}


handleXMLData :: BString -> (XML -> a) -> a
handleXMLData bs h = case parse' defaultParseOptions bs of
    Left (XMLParseError msg loc) -> throw $ XMLParserException msg
    Right xml                    -> h xml

