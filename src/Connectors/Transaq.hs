{-# LANGUAGE OverloadedStrings #-}


module Connectors.Transaq where


import Text.XML.Expat.Tree
import Text.XML.Expat.Format
import Text.XML.Expat.Cursor
import qualified Data.ByteString as B
import Connectors.Transaq.FFI

toXML :: B.ByteString -> Either XMLParseError (Node B.ByteString B.ByteString)
toXML s = parse' defaultParseOptions s

parseCommandResult s = s

{-
connect conn = do
  [command] <- xmlToString conn
  result <- sendCommand command
  parseCommandResult result

disconnect = do
  [command] <- xmlToString $ mkelem "command" [ sattr "id" "disconnect" ] []
  result <- sendCommand command
  parseCommandResult result
-}

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