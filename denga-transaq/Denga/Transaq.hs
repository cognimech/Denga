{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

-- denga, Haskell trading framework
-- Copyright (C) 2016 Leonid Vlasenkov <leo.vlasenkov@gmail.com>

-- | 

module Denga.Transaq (

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
import           Data.ByteString.Lex.Fractional
import qualified Data.Map as Map
import           Foreign (freeHaskellFunPtr)

import           Text.XML.Expat.Tree
import           Text.XML.Expat.Format
--import           Text.XML.Expat.Cursor
import           Text.XML.Expat.Proc

import           Denga.Core
import qualified Denga.Transaq.FFI as FFI


type XML = Node B.ByteString B.ByteString
type Transaq = StateT TransaqState IO

data TransaqState = TransaqState {
  _callbackFunPtr :: Maybe FFI.TCallback,
  _callbacks      :: Map.Map B.ByteString (XML -> IO Bool)
  }

makeLenses ''TransaqState

mkCallback :: Map.Map B.ByteString (XML -> IO Bool) -> B.ByteString -> IO Bool
mkCallback cbmap bs =
  case parse' defaultParseOptions bs of
    Left _    -> return False
    Right xml ->
      case Map.lookup (eName xml) cbmap of
        Just cb -> cb xml
        Nothing -> return True

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

xmlToTick :: XML -> Tick
xmlToTick xml =
  let
    Just p = findChild "price" xml
    price = (getText.head.getChildren) p
    Just (pr, str) = readDecimal price
  in Tick {tickPrice = pr}


instance DataSource Transaq where
  onTick f = do
    let
      g ts = do
        bs <- sequence $ map (f.xmlToTick) $ eChildren ts
        return $ foldl (&&) True bs
    callbacks %= Map.insert "ticks" g
    updateCallback


parseCommandResult s = return s

connect conn = do
  let command = formatNode' (conn :: XML)
  result <- FFI.sendCommand command
  parseCommandResult result

disconnect = do
  result <- FFI.sendCommand "<command id=\"disconnect\"/>"
  parseCommandResult result

getServiceInfo = do
  result <- FFI.getServiceInfo "<request><value>queue_size</value><value>queue_mem_used</value><value>version</value></request>"
  return result

initialize = FFI.initialize

unInitialize = FFI.unInitialize