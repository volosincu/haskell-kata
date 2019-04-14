
module Equals (
       Equals(..)) where

class Equals a where
      (<!!>) :: a -> a -> Bool
      x <!!> y = not (x <!!> y)