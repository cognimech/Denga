{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

-- denga, Haskell trading framework
-- Copyright (C) 2016 Leonid Vlasenkov <leo.vlasenkov@gmail.com>

-- | 

module Denga.Transaq (

  Transaq (..),
  ServiceInfo (..),
  getServiceInfo,
  initialize,
  unInitialize,
  connect,
  disconnect

  ) where

import           Control.Monad.State
import           Control.Concurrent.MVar
import           Control.Lens
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import           Data.ByteString.Lex.Fractional
import qualified Data.Map as Map
import           Data.Default
import           Foreign (freeHaskellFunPtr)
import           Text.XML.Expat.Tree
import           Text.XML.Expat.Format
import           Text.XML.Expat.Proc
import           Denga.Core
import           Denga.Transaq.Types
import qualified Denga.Transaq.FFI as FFI

import Data.Maybe (fromJust)


connect conn = do
  let command = formatNode' (conn :: XML)
  result <- FFI.sendCommand command
  parseCommandResult result

disconnect = do
  result <- FFI.sendCommand "<command id=\"disconnect\"/>"
  parseCommandResult result

getServiceInfo :: Transaq (Either (Int, B.ByteString) ServiceInfo)
getServiceInfo = do
  result <- liftIO $
    FFI.getServiceInfo "<request><value>queue_size</value><value>queue_mem_used</value><value>version</value></request>"
  case result of
    Left err  -> return $ Left err
    Right bs ->
      case parse' defaultParseOptions bs of
        Left _    -> return $ Left (0, "Parse error")
        Right xml -> return $ Right $
          ServiceInfo {
            serInfQueueSize = fromJust $ getChildText "queue_size" xml,
            serInfQueueMemUsed = fromJust $ getChildText "queue_mem_used" xml,
            serInfVersion = fromJust $ getChildText "version" xml
            }

initialize :: String -> Int -> Transaq (Maybe B.ByteString)
initialize s d = liftIO $ FFI.initialize (BC8.pack s) d

unInitialize :: Transaq (Maybe B.ByteString)
unInitialize = liftIO $ FFI.unInitialize

runTransaq :: Settings -> Transaq a -> IO a
runTransaq s algo = evalStateT (def {_settings = s}) algo

instance DataSource Transaq where

  onTick f = do
    let
      g ts = do
        bs <- sequence $ map (f.xmlToTick) $ eChildren ts
        return $ foldl (&&) True bs
    callbacks %= Map.insert "ticks" g
    updateCallback
