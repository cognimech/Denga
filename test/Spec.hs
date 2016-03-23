{-# LANGUAGE OverloadedStrings #-}

import Connectors.Transaq.FFI
import Connectors.Transaq
--import Text.XML.HXT.Core
import System.Directory
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  dir <- getCurrentDirectory
  --conn <- readFile (dir ++ "\\test\\TransaqConnection.xml")
  --let c = readString [withWarnings no] conn
  serviceInfo >>= print
  initialize (B.pack $ dir ++ "\\test\\log\\") 2 >>= print
  --connect c >>= print
  --disconnect >>= print  
  unInitialize >>= print
