{-# LANGUAGE OverloadedStrings #-}


module Connectors.Transaq where


import Text.XML.Expat.Tree
import Text.XML.Expat.Format
import Text.XML.Expat.Cursor
import qualified Data.ByteString as B
import Connectors.Transaq.FFI


parseCommandResult s = return s

connect :: Node B.ByteString B.ByteString -> IO B.ByteString
connect conn = do
  let command = formatNode' conn
  result <- sendCommand command
  parseCommandResult result

disconnect = do
  result <- sendCommand "<command id=\"disconnect\"/>"
  parseCommandResult result

serviceInfoRequest :: Node B.ByteString B.ByteString
serviceInfoRequest = Element "request" [] [
  Element "value" [] [Text "queue_size"],
  Element "value" [] [Text "queue_mem_used"],
  Element "value" [] [Text "version"]
  ]

--serviceInfo :: IO B.ByteString
serviceInfo = do
  let request = formatNode' serviceInfoRequest
  result <- getServiceInfo request
  return result