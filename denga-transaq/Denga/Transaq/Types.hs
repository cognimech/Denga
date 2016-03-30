{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE DeriveDataTypeable   #-}

-- denga, Haskell trading framework
-- Copyright (C) 2016 Leonid Vlasenkov <leo.vlasenkov@gmail.com>

-- | 

module Denga.Transaq.Types where



import           Control.Monad.State
import           Control.Exception
import           Control.Lens
import qualified Data.ByteString as B
import           Data.ByteString.Lex.Integral
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Default
import           Data.Typeable (Typeable)
import           Text.XML.Expat.Tree
import           Denga.Transaq.FFI (TCallback)



type BString = B.ByteString
type XML = Node BString BString
type Transaq = StateT TransaqState IO

uIntToBS :: Int -> BString
uIntToBS x = case packDecimal x of
    Just s    -> s
    otherwise -> throw $ TransaqError "passed negative number to command"

boolToBS :: Bool -> BString
boolToBS x = if x then "true" else "false"

data TransaqError
    = XMLParserException String
    | SendCommandException String
    | TransaqError String
    deriving (Show, Typeable)

instance Exception TransaqError


data ServiceInfo = ServiceInfo
    { serInfQueueSize    :: BString
    , serInfQueueMemUsed :: BString
    , serInfVersion      :: BString
    } deriving (Show)



data Settings = Settings
    { connection   :: !Connection
    , logDirectory :: !String
    , logLevel     :: !Int
    }

instance Default Settings where
    def = Settings
        { connection = def
        , logDirectory = ""
        , logLevel = 2
        }



data Connection = Connection
    { login           :: !BString
    , password        :: !BString
    , host            :: !BString
    , port            :: !Int
    , autopos         :: !(Maybe Bool)
    , micex_registers :: !(Maybe Bool)
    , milliseconds    :: !(Maybe Bool)
    , utc_time        :: !(Maybe Bool)
    , proxy           :: !(Maybe Proxy)
    , rqdelay         :: !(Maybe Int)
    , session_timeout :: !(Maybe Int)
    , request_timeout :: !(Maybe Int)
    , push_u_limits   :: !(Maybe Int)
    , push_pos_equity :: !(Maybe Int)
    }

instance Default Connection where
    def = Connection
        { login           = ""
        , password        = ""
        , host            = ""
        , port            = 0
        , autopos         = Nothing
        , micex_registers = Nothing
        , milliseconds    = Nothing
        , utc_time        = Nothing
        , proxy           = Nothing
        , rqdelay         = Nothing
        , session_timeout = Nothing
        , request_timeout = Nothing
        , push_u_limits   = Nothing
        , push_pos_equity = Nothing
        }

connectionToXML :: Connection -> XML
connectionToXML c = Element "command" [("id", "connect")] $
    [ Element "login" [] [Text $ login c]
    , Element "password" [] [Text $ password c]
    , Element "host" [] [Text $ host c]
    , Element "port" [] [Text $ uIntToBS $ port c]
    ] ++ optional
    where
        optional = catMaybes
            [ fmap (\x -> Element "autopos" [] [Text $ boolToBS x]) $ autopos c
            , fmap (\x -> Element "micex_registers" [] [Text $ boolToBS x]) $ micex_registers c
            , fmap (\x -> Element "milliseconds" [] [Text $ boolToBS x]) $ milliseconds c
            , fmap (\x -> Element "utc_time" [] [Text $ boolToBS x]) $ utc_time c
            , fmap (\x -> proxyToXML x) $ proxy c
            , fmap (\x -> Element "rqdelay" [] [Text $ uIntToBS x]) $ rqdelay c
            , fmap (\x -> Element "session_timeout" [] [Text $ uIntToBS x]) $ session_timeout c
            , fmap (\x -> Element "request_timeout" [] [Text $ uIntToBS x]) $ request_timeout c
            , fmap (\x -> Element "push_u_limits" [] [Text $ uIntToBS x]) $ push_u_limits c
            , fmap (\x -> Element "push_pos_equity" [] [Text $ uIntToBS x]) $ push_pos_equity c
            ]

data Proxy = Proxy
    { proxyType     :: !(Maybe BString)
    , proxyAddr     :: !(Maybe BString)
    , proxyPort     :: !(Maybe BString)
    , proxyLogin    :: !(Maybe BString)
    , proxyPassword :: !(Maybe BString)
    }

instance Default Proxy where
    def = Proxy
        { proxyType     = Nothing
        , proxyAddr     = Nothing
        , proxyPort     = Nothing
        , proxyLogin    = Nothing
        , proxyPassword = Nothing
        }

proxyToXML :: Proxy -> XML
proxyToXML p = Element "proxy" attrs []
    where
        attrs = catMaybes
            [ fmap (\x -> ("type", x)) $ proxyType p
            , fmap (\x -> ("addr", x)) $ proxyAddr p
            , fmap (\x -> ("port", x)) $ proxyPort p
            , fmap (\x -> ("login", x)) $ proxyLogin p
            , fmap (\x -> ("password", x)) $ proxyPassword p
            ]


data TransaqState = TransaqState
    { _callbackFunPtr :: !(Maybe TCallback)
    , _callbacks      :: !(Map.Map BString (XML -> IO Bool))
    , _settings       :: !Settings
    }

instance Default TransaqState where
    def = TransaqState
        { _callbackFunPtr = Nothing
        , _callbacks      = Map.empty
        , _settings       = def 
        }

makeLenses ''TransaqState
