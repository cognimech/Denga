{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}


module Connectors.Transaq where


import Text.XML.HXT.Core
import Connectors.Transaq.FFI


xmlToString xml = runX $ root [] [xml] >>> writeDocumentToString [withOutputEncoding utf8]


parseCommandResult x = return x

connect conn = do
  [command] <- xmlToString conn
  result <- sendCommand command
  parseCommandResult result

disconnect = do
  [command] <- xmlToString $ mkelem "command" [ sattr "id" "disconnect" ] []
  result <- sendCommand command
  parseCommandResult result


serviceInfoRequest  :: ArrowXml a => a XmlTree XmlTree
serviceInfoRequest = mkelem "request" []
  [
    mkelem "value" [] [ txt "queue_size" ], 
    mkelem "value" [] [ txt "queue_mem_used" ],
    mkelem "value" [] [ txt "version" ]
  ]

serviceInfo = do
  [request] <- xmlToString serviceInfoRequest
  print request
  result <- getServiceInfo request
  return result