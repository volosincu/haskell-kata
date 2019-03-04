import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map(..))
import Data.Maybe (Maybe)
import System.Environment
import Control.Applicative

codes = Map.fromList [
  ("ro", 40),
  ("uk", 44),
  ("us", 1),
  ("fr", 33),
  ("es", 34),
  ("it", 39)]

getCountryCode :: String -> Int
getCountryCode country = case codes Map.!? country of
                           Just cod -> cod
                           Nothing -> 0

getCountryCodeMaybe :: Maybe String -> Maybe Int
getCountryCodeMaybe (Just country) = let cod = getCountryCode country in (Just cod)
getCountryCodeMaybe Nothing = (Just 0)

-- Maybe is a instance of Functor typeclass so it can be mapped over using fmap

-- getCountryCode "ro"
-- 40

-- getCountryCode <$> (Just "ro")
-- Just 40

-- fmap getCountryCode (Just "fr")
-- Just 33

-- fmap getCountryCode (Just "jp")
-- Just 0


-- Maybe is an applicative Functor so we can take advantage of this to avoid all
-- the boilerplate from 'getCountryCodeMaybe' and lift the getCountryCode to
-- work with Maybe or other applicative functor.

-- liftedGetCC = liftA getCountryCode
-- liftedGetCC (Just "ro")

main = do
  args <- getArgs
  print $ unwords $ ["seach country code for country: "] ++ args
  let code = getCountryCode $ unwords args in print code

  -- using lifted function
  let country = unwords args in print $ liftedGetCC (Just country)

  -- using <$> function application
  let country = unwords args in print $ getCountryCode <$> (Just country)
  return ()
  where
    liftedGetCC = liftA getCountryCode
