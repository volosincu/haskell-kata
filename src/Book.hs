module Book (
       Book(..) ) where

import Equals

data Book = Matematica | Istorie | Medicina | Geografie deriving (Eq, Show)

instance Equals Book where
     Matematica <!!> Medicina = True
     Istorie <!!> Geografie = True
     _ <!!> _ = False