-- file Spec.hs
import Book
import Equals
import MergePoint
import Test.Hspec (hspec, describe, it)
import Test.QuickCheck

import EqualsSpec
import BookSpec
import ParseDomSpec
import MergePointSpec

main :: IO ()
main = hspec $ do
  ParseDomSpec.maxDepthSpec
  ParseDomSpec.documentToStackSpec'EdgeCases
  ParseDomSpec.documentToStackSpec'PositiveCases
  ParseDomSpec.isValidStackSpec'ValidCase
  ParseDomSpec.isValidStackSpec'NotValidCase
  MergePointSpec.nodeSpec
  MergePointSpec.listSpec