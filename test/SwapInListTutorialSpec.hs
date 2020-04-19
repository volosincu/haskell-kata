
module SwapInListTutorialSpec ( swapByValue', swapByIndex' ) where

import SwapInListTutorial ( swapByValue, swapByIndexOnChar, swapByIndexOnNum )
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.HUnit


swapByValue' :: Spec
swapByValue' = describe "Test schimbare dupa valoare" $ do
    it "Should swap 1 3" $ do
        shouldBe [3,2,1,4,5,6,7] $ swapByValue 1 3 [1,2,3,4,5,6,7]
    it "Should swap 1 2" $ do
        shouldBe [2,1,3,4,5,6,7] $ swapByValue 1 2 [1,2,3,4,5,6,7]
    it "Should swap 1 6" $ do
        shouldBe [6,2,3,4,5,1] $ swapByValue 1 6 [1,2,3,4,5,6]
    it "Should swap 4 3" $ do
        shouldBe [97,98,100,99] $ swapByValue 100 99 [97,98,99,100]
    it "Should swap 4 3" $ do
        shouldBe [117,99,8,3,100,117,1] $ swapByValue 1 117 [1,99,8,3,100,1,117]

swapByIndexOnChar' :: Spec
swapByIndexOnChar' = describe "Test schimbare valori dupa index (Char)" $ do
    it "Should swap indeces 5 6" $ do
        shouldBe resultA56 ['a','b','c','d','e','g','f']
    it "Should swap indeces 2 4" $ do
        shouldBe resultA24 ['a','b','e','d','c','f','g']
    it "Should swap indeces 4 2" $ do
        shouldBe resultB42 ['a','a','e','a','c','a','a']
    it "Should swap indeces 1 0" $ do
        shouldBe resultC10 ['b','a','a','a','a','a','a']
    it "Should swap indeces 1 0" $ do
        shouldBe [2,1,5,6,8,3,4] $ swapByIndexOnNum 1 0 [1,2,5,6,8,3,4]
    where
        inputA    = ['a','b','c','d','e','f','g']
        resultA56 = swapByIndexOnChar 6 5 inputA
        resultA24 = swapByIndexOnChar 2 4 inputA
        inputB    = ['a','a','c','a','e','a','a']
        resultB42 = swapByIndexOnChar 4 2 inputB
        inputC    = ['a','b','a','a','a','a','a']
        resultC10 = swapByIndexOnChar 1 0 inputC

swapByIndexOnNum' :: Spec
swapByIndexOnNum' = describe "Test schimbare valori dupa index (Num)" $ do
    it "Should swap indeces 1 0" $ do
        shouldBe resultA10 [2,1,5,6,8,3,4]
    it "Should swap indeces 4 2" $ do
        shouldBe resultA10 [2,1,8,6,5,3,4]
    where
        resultA10 = swapByIndexOnNum 1 0 [1,2,5,6,8,3,4]
        resultB42 = swapByIndexOnNum 4 2 [1,2,5,6,8,3,4]
