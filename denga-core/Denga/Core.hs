-- denga, Haskell trading framework
-- Copyright (C) 2016 Leonid Vlasenkov <leo.vlasenkov@gmail.com>

-- | 

module Denga.Core (
  
  Price,
  Amount,
  Direction,
  Tick (..),
  DataSource (..)

  ) where

type Price = Double
type Amount = Double
data Direction = Buy | Sell

data Tick = Tick {
  tickPrice     :: Price
  }

class (Monad c) => DataSource c where
  onTick :: (Tick -> IO Bool) -> c Bool
