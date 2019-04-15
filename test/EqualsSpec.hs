
module EqualsSpec (
       equalsBookInstanceSpec,
       equalsPartialApplicationSpec ) where

import Book
import Equals
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.HUnit

equalsBookInstanceSpec :: Spec
equalsBookInstanceSpec = describe "Test Equals Book instance - check Equals operator on book types" $ do
    it "Matematica <!!> Medicina are Equal" $ do
      Matematica <!!> Medicina `shouldBe` True
    it "Istorie <!!> Geografie are Equal" $ do
      Istorie <!!> Geografie `shouldBe` True
    it "Istorie <!!> Matematica aren't Equal" $ do
      Istorie <!!> Matematica `shouldBe` False
    it "Medicina <!!> Geografie aren't Equal" $ do
      Medicina <!!> Geografie `shouldBe` False

equalsPartialApplicationSpec :: Spec
equalsPartialApplicationSpec = describe "<!!> - partial apply" $ do
    it "(<!!> Medicina) shouldSatisfy Matematica" $ do
      (shouldSatisfy Matematica (<!!> Medicina))
    it "(<!!> Istorie) shouldSatisfy Geografie" $ do
      (shouldSatisfy Istorie (<!!> Geografie))
