{-# LANGUAGE OverloadedStrings #-}

import Denga.Transaq
import System.Directory
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.Thread.Delay
import qualified Data.ByteString.Char8 as BC8
import Private (myConn)
import           Control.Exception

printOnFail :: Transaq (Maybe String) -> Transaq ()
printOnFail action = do
    res <- action
    liftIO $ maybe (return ()) putStrLn res


algo = do
    printOnFail initialize
    printOnFail connect
    --liftIO $ delay 1000000
    printOnFail disconnect
    printOnFail unInitialize

main :: IO ()
main = do
    dir <- getCurrentDirectory
    let
        exe = runTransaq algo $ def
            { logDirectory = dir ++ "\\test\\log\\"
            , connection = myConn
            }
    catch exe $ \e -> case e of
        SendCommandException s -> putStrLn s
        otherwise -> return()


