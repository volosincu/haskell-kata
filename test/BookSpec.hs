
module BookSpec (
       bookDataContructorsSpec ) where

import Book
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.HUnit

bookDataContructorsSpec :: Spec
bookDataContructorsSpec = describe "Book data type" $ do
    it "Matematica" $ do
      Matematica `shouldBe` (Matematica::Book)
    it "Medicina" $ do
      Medicina `shouldBe` (Medicina::Book)
    it "Geografie" $ do
      Geografie `shouldBe` (Geografie::Book)
    it "Istorie" $ do
      Istorie `shouldBe` (Istorie::Book)