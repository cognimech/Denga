{-# LANGUAGE OverloadedStrings #-}

module Private (myConn) where
import Denga.Transaq

myConn = def
    { login           = ""
    , password        = ""
    , host            = ""
    , port            = 0
    }