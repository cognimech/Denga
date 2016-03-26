-- denga, Haskell trading framework
-- Copyright (C) 2016 Leonid Vlasenkov <leo.vlasenkov@gmail.com>

-- | 

module Denga.Core (
  
  Price,
  Amount,
  Direction,
  Tick (..),
  SequrityC,
  DataSource (..)

  ) where

type Price = Double
type Amount = Double
data Direction = Buy | Sell

data Tick s = Tick {
  tickPrice     :: Price,
  tickSequrity  :: s
  }

class (Eq s) => SequrityC s

class (Monad c) => DataSource c where
  onTick :: (SequrityC s) => (Tick s -> IO Bool) -> c Bool
