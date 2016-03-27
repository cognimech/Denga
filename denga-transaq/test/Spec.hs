{-# LANGUAGE OverloadedStrings #-}

import Denga.Transaq

import System.Directory
import qualified Data.ByteString as B
import Text.XML.Expat.Tree
import Control.Concurrent.Thread.Delay



algo = do
  initialize
  connect
  liftIO $ delay 1000000
  disconnect
  unInitialize

main :: IO ()
main = do
  dir <- getCurrentDirectory
  conn <- B.readFile (dir ++ "\\test\\TransaqConnection.xml")
  let Right c = parse' defaultParseOptions conn :: Either XMLParseError (Node B.ByteString B.ByteString)
  runTransaq def algo

