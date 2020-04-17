module HackerRank.ChaosQueueSpec (minimumBribes') where

import Debug.Trace
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.HUnit

import HackerRank.ChaosQueue (minimumBribes)

minimumBribes' :: Spec
minimumBribes' = describe "test on small arrays" $ do
    it "Should compute 3 steps " $ do
        result <- minimumBribes [2,1,5,3,4]
        shouldBe result "3"
    it "Should return Too chaotic" $ do
        result <- minimumBribes [2,1,5,3,4,6]
        shouldBe result "Too chaotic"
    it "Should return Too chaotic" $ do
        result <- minimumBribes [2,1,4,3,5,9,6,7,8]
        shouldBe result "Too chaotic"
    it "Should compute 7 steps " $ do
        result <- minimumBribes [3,1,2,5,7,8,6,4]
        shouldBe result "7"
