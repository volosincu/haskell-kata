import Control.Monad
import qualified Kata.DataTypes as KDT
import Kata.DataTypes (
  Value(..)
  , Height(..)
  , User(..)
  , PendingTx(..) )
import System.Environment

donatie :: Value Int
donatie = Value 19

termen :: Height Int
termen = Height 3

us010 :: User
us010 = User {
  uuid = "01010101010"
  , username = "mihai@mail.com" }

platax :: PendingTx
platax = PendingTx {
  contrib = donatie
  , sender = us010
  , deadline = termen }

-- let u (User _ admin) = admin in u us010
-- let mail (PendingTx _ (User _ username) _) = username in mail platax

main = do
  args <- getArgs
  (print "a test")
  --c <- getChar
  --when (c /= ' ') $ do
  --  putChar c
  --  main
