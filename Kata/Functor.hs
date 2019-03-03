import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map(..))
import Data.Maybe (Maybe)
import System.Environment

codes = Map.fromList [
  ("ro", 40),
  ("uk", 44),
  ("us", 1),
  ("fr", 33),
  ("es", 34),
  ("it", 39)
]

getCountryCode :: String -> Int
getCountryCode country = case codes Map.!? country of
                           Just cod -> cod
                           Nothing -> 0

getCountryCodeMaybe :: Maybe String -> Maybe Int
getCountryCodeMaybe (Just country) = let cod = getCountryCode country in (Just cod)
getCountryCodeMaybe Nothing = (Just 0)

-- fmap getCountryCode (Just "ro")
-- Just 40

-- fmap getCountryCode (Just "fr")
-- Just 33

-- fmap getCountryCode (Just "jp")
-- Just 0
