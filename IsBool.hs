{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module IsBool where

import Prelude hiding (not)
import qualified Prelude as P

class Eq a => True a where
  true :: a

instance True Bool where
  true = True

instance Integral a => True a where
  true = 1

class Eq a => False a where
  false :: a

instance False Bool where
  false = False

instance Integral a => False a where
  false = 0

class (Eq b, True b, False b) => IsBool b where { }

fromBool :: IsBool b => Bool -> b
fromBool True = true
fromBool False = false

toBool :: IsBool b => b -> Bool
toBool = (/=) false
