
module Equals (
       (<!!>) ) where

import Book

class Equals a where
      (<!!>) :: a -> a -> Bool
      x <!!> y = not (x <!!> y)

instance Equals Book where
     Matematica <!!> Medicina = True
     Istorie <!!> Geografie = True
     _ <!!> _ = False