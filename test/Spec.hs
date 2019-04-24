-- file Spec.hs
import Book
import Equals
import Test.Hspec (hspec, describe, it)
import Test.QuickCheck

import EqualsSpec
import BookSpec
import ParseDomSpec

main :: IO ()
main = hspec $ do
  ParseDomSpec.documentToStackSpec'EdgeCases
  ParseDomSpec.documentToStackSpec'PositiveCases
  ParseDomSpec.isValidStackSpec'ValidCase
  ParseDomSpec.isValidStackSpec'NotValidCase