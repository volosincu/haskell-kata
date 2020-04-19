-- file Spec.hs
import Book
import Equals
import MergePoint
import Locker
import Test.Hspec (hspec, describe, it)
import Test.QuickCheck

import HackerRank.ChaosQueueSpec
import LockerSpec
import EqualsSpec
import BookSpec
import ParseDomSpec
import MergePointSpec
import SwapInListTutorialSpec

main :: IO ()
main = hspec $ do
  ParseDomSpec.maxDepthSpec
  ParseDomSpec.documentToStackSpec'EdgeCases
  ParseDomSpec.documentToStackSpec'PositiveCases
  ParseDomSpec.isValidStackSpec'ValidCase
  ParseDomSpec.isValidStackSpec'NotValidCase
  MergePointSpec.nodeSpec
  MergePointSpec.listSpec
  MergePointSpec.mergePointSpec
  HackerRank.ChaosQueueSpec.minimumBribes'
  SwapInListTutorialSpec.swapByValue'
  SwapInListTutorialSpec.swapByIndexOnChar'
  SwapInListTutorialSpec.swapByIndexOnNum'
  ---LockerSpec.lockerCrudOperationsSpec
