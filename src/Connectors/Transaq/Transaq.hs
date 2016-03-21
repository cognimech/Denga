{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}


module Connectors.Transaq where


import Text.XML.HXT.Core
import FFI

parseCommandResult x = x

connectWith xs = do
  command <- runX $ (mkelem "command" [ sattr "id" "connect" ] xs) >>> writeDocumentToString [withOutputEncoding utf8]
  result <- sendCommand command
  parseCommandResult result

disconnect = do
  command <- runX $ (mkelem "command" [ sattr "id" "disconnect" ] []) >>> writeDocumentToString [withOutputEncoding utf8]
  result <- sendCommand command
  parseCommandResult result


serviceInfoRequest  :: ArrowXml a => a XmlTree XmlTree
serviceInfoRequest = mkelem "request" []
  [
    mkelem "value" [] [ txt "queue_size" ], 
    mkelem "value" [] [ txt "queue_mem_used" ],
    mkelem "value" [] [ txt "connector_version" ]
  ]

serviceInfo = do
  request <- runX $ serviceInfoRequest >>> writeDocumentToString [withOutputEncoding utf8]
  result <- getServiceInfo request
  return result