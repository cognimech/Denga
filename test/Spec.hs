{-# LANGUAGE OverloadedStrings #-}

import Connectors.Transaq.FFI
import Connectors.Transaq
--import Text.XML.HXT.Core
import System.Directory
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import Text.XML.Expat.Tree

main :: IO ()
main = do
  dir <- getCurrentDirectory
  conn <- B.readFile (dir ++ "\\test\\TransaqConnection.xml")
  let Right c = parse' defaultParseOptions conn :: Either XMLParseError (Node B.ByteString B.ByteString)
  serviceInfo >>= print
  initialize (B8.pack $ dir ++ "\\test\\log\\") 2 >>= print
  connect c >>= print
  disconnect >>= print  
  unInitialize >>= print
