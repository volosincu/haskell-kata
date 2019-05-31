module LockerSpec ( lockerCrudOperationsSpec ) where

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy, beforeAll, before, before_)
import Test.HUnit

import Locker ( Locker(..), getLockerState, addLocker, removeLocker )

i :: [Int]
i = [0]

bf :: IO ()
bf = putStrLn "before each locker test ..."

lockerCrudOperationsSpec :: Spec
lockerCrudOperationsSpec = do
     before_ bf $ do
         describe "Check crud operations of Locker" $ do
             it "getLockerState" $ do
                 let
                     res :: Locker String
                     res = getLockerState 1

                     (Taken code) = res
                     in code `shouldBe` "1234" 

             it "addLocker" $ do
                 let
                     lockerCode :: String
                     lockerCode = "1234"

                     locker = addLocker lockerCode

                     (Taken code) = locker
                     in code `shouldBe` lockerCode 

             it "removeLocker" $ do
                 let
                     lockerIndex :: Int
                     lockerIndex = 1

                     lockerCode :: String
                     lockerCode = "1234"

                     locker = removeLocker 1 lockerCode

                     (Taken code) = locker
                     in code `shouldBe` "1234"
