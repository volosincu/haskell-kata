module HackerRank.ChaosQueueSpec (minimumBribes', minimumBribes'') where

import System.IO
import Test.HUnit
import Debug.Trace
import Data.List as List
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy, beforeAll)
import HackerRank.ChaosQueue (minimumBribes)

minimumBribes' :: Spec
minimumBribes' = describe "test on small arrays" $ do
        it "Should compute 3 steps " $ do
            result <- minimumBribes [2,1,5,3,4]
            shouldBe result "3"
        it "Should return Too chaotic" $ do
            result <- minimumBribes [5,1,2,3,4,6]
            shouldBe result expectedChaotic
        it "Should return Too chaotic" $ do
            result <- minimumBribes [2,1,4,3,5,9,6,7,8]
            shouldBe result expectedChaotic
        it "Should compute 8 steps " $ do
            result <- minimumBribes [3,1,2,5,7,8,6,4]
            shouldBe result "8"
        where
            expectedChaotic = "Too chaotic"

minimumBribes'' :: Spec
minimumBribes'' = describe "test on big arrays" $ do
        it "Should compute 1201 steps " $ do
            inpStr <- readFile "./test/HackerRank/test_data/ChaosQueueSpecA.txt"
            result <- minimumBribes $ toIntegral inpStr
            shouldBe result "1201"
        it "Should compute 1208 steps " $ do
            inpStr <- readFile "./test/HackerRank/test_data/ChaosQueueSpecB.txt"
            result <- minimumBribes $ toIntegral inpStr
            shouldBe result "1208"
        it "Should compute Too chaotic " $ do
            inpStr <- readFile "./test/HackerRank/test_data/ChaosQueueSpecC.txt"
            result <- minimumBribes $ toIntegral inpStr
            shouldBe result expectedChaotic
        it "Should compute 1196 steps " $ do
            inpStr <- readFile "./test/HackerRank/test_data/ChaosQueueSpecD.txt"
            result <- minimumBribes $ toIntegral inpStr
            shouldBe result "1196"
        it "Should compute Too chaotic steps " $ do
            inpStr <- readFile "./test/HackerRank/test_data/ChaosQueueSpecE.txt"
            result <- minimumBribes $ toIntegral inpStr
            shouldBe result expectedChaotic
        it "Should compute 1210 steps " $ do
            inpStr <- readFile "./test/HackerRank/test_data/ChaosQueueSpecF.txt"
            result <- minimumBribes $ toIntegral inpStr
            shouldBe result "1210"
        where
            expectedChaotic = "Too chaotic"
            toIntegral :: String -> [Int]
            toIntegral = map read . words