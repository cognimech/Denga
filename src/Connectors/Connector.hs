module Connector where

type Price = Double
type Amount = Double
data Direction = Buy | Sell

class (Eq s) => Sequrity s

class (Monad c) => Connector c where
  placeLimitOrder :: (Sequrity s) => s -> Direction -> Price -> Amount -> c ()
  placeMarketOrder :: (Sequrity s) => s -> Direction -> Amount -> c ()
  tickPrice :: (Sequrity s) => s -> c Price