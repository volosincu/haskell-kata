

data Book = Matematica | Istorie | Medicina | Geografie deriving (Show)

class Equals a where
      iseq :: a -> a -> Bool
      x `iseq` y = not (x `iseq` y)

instance Equals Book where
     ieq Matematica Medicina = True
     ieq Istorie Geografie = True
     ieq _ _ = False
