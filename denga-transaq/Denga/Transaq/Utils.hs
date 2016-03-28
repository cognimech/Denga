{-# LANGUAGE OverloadedStrings    #-}

-- denga, Haskell trading framework
-- Copyright (C) 2016 Leonid Vlasenkov <leo.vlasenkov@gmail.com>

-- | 

module Denga.Transaq.Utils
  ( updateCallback
  , getChildText
  , xmlToTick
  , parseCommandResult
  ) where

import           Control.Lens
import qualified Data.ByteString as B
import           Data.ByteString.Lex.Fractional
import qualified Data.Map as Map
import           Foreign (freeHaskellFunPtr)
import           Text.XML.Expat.Tree
import           Text.XML.Expat.Format
import           Text.XML.Expat.Proc
import           Denga.Core
import           Denga.Transaq.Types
import qualified Denga.Transaq.FFI as FFI


-- | Make single callback function to pass to 'FFI.setCallback' by partial appliance to a 'Map.Map'
-- of separate user-specified callbacks.
mkCallback :: Map.Map BString (XML -> IO Bool) -> BString -> IO Bool
mkCallback cbmap bs =
  case parse' defaultParseOptions bs of
    Left _    -> return False
    Right xml ->
      case Map.lookup (eName xml) cbmap of
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
  in Tick {tickPrice = pr}

-- | Parse command result returning either success or error message
parseCommandResult s = return s

connectionToXML :: Connection -> XML
connectionToXML c = 