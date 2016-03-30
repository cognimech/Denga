{-# LANGUAGE OverloadedStrings #-}

import Denga.Transaq
import System.Directory
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.Thread.Delay

import Private (myConn)

algo = do
    initialize
    connect
    liftIO $ delay 1000000
    disconnect
    unInitialize

main :: IO ()
main = do
    dir <- getCurrentDirectory
    runTransaq algo $ def
        { logDirectory = dir ++ "\\log\\"
        , connection = myConn
        }
    return ()
