module Algorithms.SortingTutorialSpec (quicksort') where

import System.IO
import Test.HUnit
import Debug.Trace
import Data.List (sort)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy, beforeAll)
import Algorithms.SortingTutorial (quicksort)

quicksort' :: Spec
quicksort' = describe "Quicksort - sortare rapida" $ do
        it "Sorteaza elemente unice" $ do
            let result = quicksort inputA
                in shouldBe result $ sort inputA
        it "Sorteaza cu elemente duplicate" $ do
            let result = quicksort inputB
                in shouldBe result $ sort inputB
        it "Sorteaza cu doar 2 elemente" $ do
            let result = quicksort inputC
                in shouldBe result $ sort inputC
        it "Sorteaza cu doar 2 elemente ordonate" $ do
            let result = quicksort inputD
                in shouldBe result inputD
        it "Sorteaza caractere" $ do
            let result = quicksort inputE
                in shouldBe result $ sort inputE
        where
            inputA = [2,1,5,3,4,100,99,542,13,934,257,512,6]
            inputB = [512,2,6,5,3,4,100,99,542,13,934,100,257,512,6,1]
            inputC = [2,1]
            inputD = [1,2]
            inputE = ['g','c','b','e','f','a','d','z']