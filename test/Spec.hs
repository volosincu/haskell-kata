-- file Spec.hs
import Book
import Equals
import MergePoint
import Locker
import Test.Hspec (hspec, describe, it)
import Test.QuickCheck

import HackerRank.ChaosQueueSpec (minimumBribes', minimumBribes'')
import LockerSpec
import EqualsSpec
import BookSpec
import ParseDomSpec (
    maxDepthSpec,
    documentToStackSpec'EdgeCases,
    documentToStackSpec'PositiveCases,
    isValidStackSpec'ValidCase,
    isValidStackSpec'NotValidCase
    )
import MergePointSpec (nodeSpec, listSpec, mergePointSpec)
import Algorithms.SwapInListTutorialSpec (swapByValue', swapByIndexOnChar', swapByIndexOnNum')
import Algorithms.SortingTutorialSpec (quicksort')

main :: IO ()
main = hspec $ do
-- ParseDomSpec
  maxDepthSpec
  documentToStackSpec'EdgeCases
  documentToStackSpec'PositiveCases
  isValidStackSpec'ValidCase
  isValidStackSpec'NotValidCase
-- MergePointSpec
  nodeSpec
  listSpec
  mergePointSpec
-- HackerRank.ChaosQueueSpec
  minimumBribes'
  minimumBribes''
-- Algorithms.SwapInListTutorialSpec
  swapByValue'
  swapByIndexOnChar'
  swapByIndexOnNum'
-- Algorithms.SortingTutorialSpec
  quicksort'
  ---LockerSpec.lockerCrudOperationsSpec
