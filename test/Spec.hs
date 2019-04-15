-- file Spec.hs
import Book
import Equals
import Test.Hspec (hspec, describe, it)
import Test.QuickCheck

import EqualsSpec
import BookSpec

main :: IO ()
main = hspec $ do
  BookSpec.bookDataContructorsSpec
  EqualsSpec.equalsBookInstanceSpec
  EqualsSpec.equalsPartialApplicationSpec