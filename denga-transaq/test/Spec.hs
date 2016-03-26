{-# LANGUAGE OverloadedStrings #-}

import Connectors.Transaq.FFI
import Connectors.Transaq

import System.Directory
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import Text.XML.Expat.Tree
import Control.Concurrent.Thread.Delay


import Foreign --(freeHaskellFunPtr, nullPtr)

foo :: B.ByteString -> IO Bool
foo s = do
  print "foo"
  return True

--bar :: MVar a -> B.ByteString -> IO Bool
bar m s = do
  val <- readMVar m
  print val
  return True

main :: IO ()
main = do
  dir <- getCurrentDirectory
  conn <- B.readFile (dir ++ "\\test\\TransaqConnection.xml")
  let Right c = parse' defaultParseOptions conn :: Either XMLParseError (Node B.ByteString B.ByteString)
  --serviceInfo >>= print
  m <- newMVar [123]
  initialize (B8.pack $ dir ++ "\\test\\log\\") 2 >>= print
  connect c >>= print
  Just fp0 <- setCallback $ bar m
  delay 1000000
  takeMVar m
  putMVar m [0]
  --freeHaskellFunPtr fp0
  delay 1000000
  disconnect >>= print
  --freeHaskellFunPtr fp1 
  unInitialize >>= print



f1 :: MVar a -> ByteString -> IO Bool
f2 :: MVar b -> ByteString -> IO Bool
f3 :: MVar c -> ByteString -> IO Bool

ff :: MVar a -> MVar b -> MVar c -> ByteString -> IO Bool
ff a b c = case ByteString of
  1 -> f1 a ByteString
  2 -> f2 b ByteString
  3 -> f3 c ByteString

setCallback $ ff mv1 mv2 mv3


g :: [(MVar x -> ByteString -> IO Bool)] -> ()