{-# LANGUAGE OverloadedStrings #-}

import Connectors.Transaq.FFI
import Connectors.Transaq

import System.Directory
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import Text.XML.Expat.Tree
import Control.Concurrent.Thread.Delay

import Foreign (freeHaskellFunPtr)

foo :: B.ByteString -> IO Bool
foo s = do
  print "foo"
  return True

bar :: B.ByteString -> IO Bool
bar s = do
  print "bar"
  return True

main :: IO ()
main = do
  dir <- getCurrentDirectory
  conn <- B.readFile (dir ++ "\\test\\TransaqConnection.xml")
  let Right c = parse' defaultParseOptions conn :: Either XMLParseError (Node B.ByteString B.ByteString)
  serviceInfo >>= print
  initialize (B8.pack $ dir ++ "\\test\\log\\") 2 >>= print
  Just fp0 <- setCallback foo
  connect c >>= print
  delay 1000000
  disconnect >>= print
  freeHaskellFunPtr fp0 
  unInitialize >>= print
