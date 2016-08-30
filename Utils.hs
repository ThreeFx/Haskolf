{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module Utils (module IsBool, module Utils) where

import IsBool

import Prelude hiding ((+),(-),(*),(/))
import qualified Prelude as P

class Semiring a where
  (<>) :: a -> a -> a
  identity :: a

instance Semiring [a] where
  (<>) = (++)
  identity = []

instance Num a => Semiring a where
  (<>) = (P.+)
  identity = 0
