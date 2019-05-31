
module Locker ( Locker(..), getLockerState, addLocker, removeLocker ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map(..))
import Data.Maybe (Maybe)
import Control.Applicative

data Locker code = Free | Taken code deriving (Ord, Eq, Show)

type LockerTable = Map.Map Int (Locker String)

getLockerState :: Int -> (Locker String)
getLockerState i = Free

addLocker :: String -> (Locker String)
addLocker code = Free

removeLocker :: Int ->  String -> (Locker String)
removeLocker index code = Free







