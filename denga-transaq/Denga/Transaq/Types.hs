{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}

-- denga, Haskell trading framework
-- Copyright (C) 2016 Leonid Vlasenkov <leo.vlasenkov@gmail.com>

-- | 

module Denga.Transaq.Types
  ( Transaq (..)
  , ServiceInfo (..)
  , Settings (..)
  , Connection (..)
  , XML
  , BString
  ) where

import           Control.Monad.State
import           Control.Lens
import qualified Data.ByteString as B
import qualified Data.Map as Map
import           Data.Default
import           Text.XML.Expat.Tree
import           Denga.Transaq.FFI (TCallback)


type BString = B.ByteString
type XML = Node BString BString
type Transaq = StateT TransaqState IO

data TransaqState = TransaqState
  { _callbackFunPtr :: Maybe TCallback
  , _callbacks      :: Map.Map BString (XML -> IO Bool)
  , _settings       :: Settings
  }

instance Default TransaqState where
  def = TransaqState
    { _callbackFunPtr = Nothing
    , _callbacks = Map.empty
    , _settings = def 
    }

makeLenses ''TransaqState


data ServiceInfo = ServiceInfo
  { serInfQueueSize    :: BString
  , serInfQueueMemUsed :: BString
  , serInfVersion      :: BString
  } deriving (Show)


data Settings = Settings
  { connection   :: Connection
  , logDirectory :: String
  , logLevel     :: Int
  }

instance Default Settings where
  def = Settings
    { connection = def
    , logDirectory = ""
    , logLevel = 2
    }


data Connection = Connection
  { login    :: BString
  , password :: BString
  , host     :: BString
  , port     :: BString
  }

instance Default Connection where
  def = Connection
    { login = ""
    , password = ""
    , host = ""
    , port = ""
    }



